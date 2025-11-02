{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Flow.Parser.Expr where

import Data.Maybe (fromJust)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty qualified as NonEmptyVector
import Text.Megaparsec qualified as Megaparsec

import Flow.AST.Surface (
  Expression (..),
  Pattern (..),
  PatternSimple (..),
  Statement (..),
  Type (..),
 )
import Flow.AST.Surface.Common (SimpleVarIdentifier (..))
import Flow.AST.Surface.Constraint (
  AnyTypeIdentifier (..),
  AnyVarIdentifier (..),
  BindersF (..),
 )
import Flow.AST.Surface.Expr (
  AppArgsF (..),
  AppF (..),
  ArgNamedF (..),
  ExpressionF (..),
  LambdaArgF (..),
  LambdaF (..),
  LambdaFullF (..),
  LambdaShortF (..),
 )
import Flow.AST.Surface.Literal (Literal)
import Flow.AST.Surface.Syntax (
  CodeBlockF (..),
  WithF (..),
  WithLhsF (..),
  WithRhsF (..),
  WithStatementF (..),
 )
import Flow.AST.Surface.Syntax qualified as Syntax
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (
  HasAnn,
  Parser,
  SourceRegion (..),
  WithPos (..),
  pEffectsResult,
  simpleVarIdentifier,
  single,
 )
import Flow.Parser.Constraint (
  anyTypeIdentifier,
  anyVarIdentifier,
  pBindersApp,
  pBindersWConstraints,
  pWhereBlockHead,
 )
import Flow.Parser.Literal (literal)
import Flow.Parser.Operators (pOperators)
import Flow.Parser.Syntax (pCodeBlock, pIfExpression, pLoopExpression, pMatchExpression)

pWildcard :: Parser SourceRegion
pWildcard = do
  tok <- single (Lexer.Punctuation Lexer.Underscore)
  pure tok.region

pLiteral :: Parser (Literal, SourceRegion)
pLiteral = literal

pParens ::
  Parser (expr Lexer.SourceRegion) ->
  Parser (expr Lexer.SourceRegion, Lexer.SourceRegion)
pParens expr = do
  tokS <- single (Lexer.Punctuation Lexer.LeftParen)
  expr' <- expr
  tokE <- single (Lexer.Punctuation Lexer.RightParen)
  pure (expr', SourceRegion{start = tokS.region.start, end = tokE.region.end})

pVar ::
  (HasAnn ty SourceRegion) =>
  Parser (ty SourceRegion) ->
  Parser (AnyVarIdentifier ty Lexer.SourceRegion)
pVar = anyVarIdentifier

pOfTypeSuffix ::
  Parser (ty SourceRegion) ->
  expr SourceRegion ->
  Parser (expr SourceRegion, ty SourceRegion)
pOfTypeSuffix pTy expr = Megaparsec.label "of type" do
  _ <- single (Lexer.Punctuation Lexer.Colon)
  ty' <- pTy
  pure (expr, ty')

pConstructor ::
  (HasAnn ty SourceRegion) =>
  Parser (ty SourceRegion) ->
  Parser (AnyTypeIdentifier ty SourceRegion)
pConstructor = anyTypeIdentifier

pIndexSuffix ::
  Parser (expr SourceRegion) ->
  expr SourceRegion ->
  Parser (expr SourceRegion, expr SourceRegion, SourceRegion)
pIndexSuffix pExpr expr = do
  tokS <- single (Lexer.Punctuation Lexer.LeftBracket)
  idxExpr <- pExpr
  tokE <- single (Lexer.Punctuation Lexer.RightBracket)
  pure
    ( expr
    , idxExpr
    , SourceRegion
        { start = tokS.region.start
        , end = tokE.region.end
        }
    )

pDotAccessSuffix ::
  (HasAnn ty SourceRegion) =>
  Parser (ty SourceRegion) ->
  expr SourceRegion ->
  Parser (expr SourceRegion, AnyVarIdentifier ty SourceRegion, SourceRegion)
pDotAccessSuffix pTy expr = do
  tokS <- single (Lexer.Punctuation Lexer.Dot)
  field <- anyVarIdentifier pTy
  pure (expr, field, SourceRegion{start = tokS.region.start, end = field.ann.end})

pAppSuffix ::
  (HasAnn ty SourceRegion, HasAnn expr SourceRegion) =>
  Parser (ty SourceRegion) ->
  Parser (expr SourceRegion) ->
  expr SourceRegion ->
  Parser (AppF ty expr SourceRegion)
pAppSuffix pTy pExpr expr = Megaparsec.label "app suffix" do
  typeParams <- Megaparsec.optional (pBindersApp pTy)
  (args, end) <-
    Megaparsec.choice
      [ pUnnamedArgs
      , pNamedArgs
      ]
  withEnd <- Megaparsec.optional pWithPost
  let ann =
        SourceRegion
          { start = expr.ann.start
          , end = case withEnd of
              Just (_, end') -> end'
              Nothing -> end
          }
  pure
    AppF
      { callee = expr
      , typeParams
      , args
      , with = fmap fst withEnd
      , ann
      }
 where
  pUnnamedArgs =
    Megaparsec.choice
      [ do
          _ <- single (Lexer.Punctuation Lexer.LeftParen)
          args <- Megaparsec.sepEndBy pExpr (single (Lexer.Punctuation Lexer.Comma))
          tokE <- single (Lexer.Punctuation Lexer.RightParen)
          pure (AppArgsUnnamedF (Vector.fromList args), tokE.region.end)
      , do
          tokE <- single (Lexer.Punctuation Lexer.LeftRightParen)
          pure (AppArgsUnnamedF Vector.empty, tokE.region.end)
      ]

  pNamedArgs = do
    _ <- single (Lexer.Punctuation Lexer.LeftBrace)
    args <- Megaparsec.sepEndBy pNamedArg (single (Lexer.Punctuation Lexer.Comma))
    tokE <- single (Lexer.Punctuation Lexer.RightBrace)
    pure (AppArgsNamedF (Vector.fromList args), tokE.region.end)

  pNamedArg = do
    name <- simpleVarIdentifier
    value <- Megaparsec.optional do
      _ <- single (Lexer.Punctuation Lexer.Assign)
      pExpr
    pure $
      ArgNamedF
        { name = name
        , value = value
        , ann =
            SourceRegion
              { start = name.ann.start
              , end = case value of
                  Just value' -> value'.ann.end
                  Nothing -> name.ann.end
              }
        }

  pWithPost = do
    _ <- single (Lexer.Keyword Lexer.With)
    _ <- single (Lexer.Punctuation Lexer.LeftBrace)
    effects <-
      Megaparsec.sepEndBy1
        (pWithStatement pTy pExpr)
        (single (Lexer.Punctuation Lexer.Comma))
    tokE <- single (Lexer.Punctuation Lexer.RightBrace)
    pure (fromJust $ NonEmptyVector.fromList effects, tokE.region.end)

pWith ::
  (HasAnn stmt SourceRegion, HasAnn ty SourceRegion, HasAnn expr SourceRegion) =>
  Parser (stmt SourceRegion) ->
  Parser (ty SourceRegion) ->
  Parser (expr SourceRegion) ->
  Parser (WithF stmt ty expr SourceRegion)
pWith pStmt pTy pExpr = do
  tokS <- single (Lexer.Keyword Lexer.With)
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  statements <- Megaparsec.sepEndBy1 (pWithStatement pTy pExpr) (single (Lexer.Punctuation Lexer.Semicolon))
  _ <- single (Lexer.Punctuation Lexer.RightBrace)
  _ <- single (Lexer.Keyword Lexer.In)
  block <- pCodeBlock pStmt pExpr
  pure
    WithF
      { statements = fromJust $ NonEmptyVector.fromList statements
      , block
      , ann = SourceRegion{start = tokS.region.start, end = block.ann.end}
      }

pTuple ::
  Parser (Expression SourceRegion) ->
  Parser (Expression SourceRegion)
pTuple pExpr = do
  tokS <- single (Lexer.Punctuation Lexer.LeftParen)
  expr1 <- pExpr
  _ <- single (Lexer.Punctuation Lexer.Comma)
  restExprs <- Megaparsec.sepEndBy1 pExpr (single (Lexer.Punctuation Lexer.Comma))
  tokE <- single (Lexer.Punctuation Lexer.RightParen)
  pure
    Expression
      { expr = ETupleF expr1 (fromJust $ NonEmptyVector.fromList restExprs)
      , ann = SourceRegion{start = tokS.region.start, end = tokE.region.end}
      }

pWithStatement ::
  (HasAnn ty SourceRegion, HasAnn expr SourceRegion) =>
  Parser (ty SourceRegion) ->
  Parser (expr SourceRegion) ->
  Parser (WithStatementF ty expr SourceRegion)
pWithStatement pTy pExpr = do
  let_ <- Megaparsec.optional (single (Lexer.Keyword Lexer.Let))
  (lhs, start) <- pWithLhs
  _ <- single (Lexer.Punctuation Lexer.Assign)
  (rhs, end) <- pWithRhs
  pure $
    WithStatementF
      { let_ = fmap (.region) let_
      , lhs = lhs
      , rhs = rhs
      , ann = SourceRegion{start = maybe start (.region.start) let_, end = end}
      }
 where
  pWithLhs =
    Megaparsec.choice
      [ pWithLhsLabelled
      , pWithLhsUnlabelled
      ]
   where
    pWithLhsLabelled = do
      name <- simpleVarIdentifier
      ty <- Megaparsec.optional do
        _ <- single (Lexer.Punctuation Lexer.Colon)
        pTy
      pure
        ( WLhsLabelled name ty
        , name.ann.start
        )
    pWithLhsUnlabelled = do
      ty <- pTy
      pure (WLhsUnlabelled ty, ty.ann.start)

  pWithRhs =
    Megaparsec.choice
      [ pWithRhsExpr
      , pWithRhsType
      ]
   where
    pWithRhsExpr = do
      expr' <- pExpr
      pure (WRhsExprF expr', expr'.ann.end)
    pWithRhsType = do
      ty <- pTy
      pure (WRhsTypeF ty, ty.ann.end)

pLambdaShort ::
  (HasAnn ty SourceRegion, HasAnn expr SourceRegion) =>
  Parser (ty SourceRegion) ->
  Parser (expr SourceRegion) ->
  Parser (LambdaF stmt ty expr SourceRegion, SourceRegion)
pLambdaShort pTy pExpr = do
  (argsOpen, args) <-
    Megaparsec.choice
      [ do
          argsOpen <- single (Lexer.Punctuation Lexer.Pipe)
          args <- Megaparsec.sepBy (pLambdaArg pTy) (single (Lexer.Punctuation Lexer.Comma))
          _ <- single (Lexer.Punctuation Lexer.Pipe)
          pure (argsOpen, args)
      , do
          argsOpenClose <- single (Lexer.Punctuation Lexer.Or)
          pure (argsOpenClose, [])
      ]
  body <- pExpr
  pure
    ( LamShortF
        LambdaShortF
          { args = Vector.fromList args
          , body = body
          , ann = SourceRegion{start = argsOpen.region.start, end = body.ann.end}
          }
    , SourceRegion{start = argsOpen.region.start, end = body.ann.end}
    )

pLambdaFull ::
  (HasAnn stmt SourceRegion, HasAnn ty SourceRegion, HasAnn expr SourceRegion) =>
  Parser (stmt SourceRegion) ->
  Parser (ty SourceRegion) ->
  Parser (expr SourceRegion) ->
  Parser (LambdaF stmt ty expr SourceRegion, SourceRegion)
pLambdaFull pStmt pTy pExpr = do
  mBinders <- Megaparsec.optional (pBindersWConstraints pTy)
  argsOpen <- single (Lexer.Punctuation Lexer.Pipe)
  args <- Megaparsec.sepBy (pLambdaArg pTy) (single (Lexer.Punctuation Lexer.Comma))
  _ <- single (Lexer.Punctuation Lexer.Pipe)
  effectsResult <- Megaparsec.optional (pEffectsResult pTy)
  whereBlock <- Megaparsec.optional (pWhereBlockHead pTy)
  body <- pCodeBlock pStmt pExpr
  let ann =
        SourceRegion
          { start = case mBinders of
              Just binders -> binders.ann.start
              Nothing -> argsOpen.region.start
          , end = body.ann.end
          }
  pure
    ( LamFullF
        LambdaFullF
          { typeParams = mBinders
          , args = Vector.fromList args
          , effectsResult = effectsResult
          , whereBlock = whereBlock
          , body = body
          , ann = ann
          }
    , ann
    )

pLambdaArg ::
  (HasAnn ty SourceRegion) =>
  Parser (ty SourceRegion) ->
  Parser (LambdaArgF ty SourceRegion)
pLambdaArg pTy = do
  mut <- Megaparsec.optional (single (Lexer.Keyword Lexer.Mut))
  name <- simpleVarIdentifier
  type_ <- Megaparsec.optional do
    _ <- single (Lexer.Punctuation Lexer.Colon)
    pTy
  pure $
    LambdaArgF
      { mut = fmap (.region) mut
      , name
      , type_
      , ann =
          SourceRegion
            { start = case mut of
                Just mut' -> mut'.region.start
                Nothing -> name.ann.start
            , end = case type_ of
                Just ty -> ty.ann.end
                Nothing -> name.ann.end
            }
      }

-- | Top-level expression parser (stub for TDD phase)
pExpression ::
  Parser (Statement SourceRegion) ->
  Parser (PatternSimple SourceRegion) ->
  Parser (Pattern SourceRegion) ->
  Parser (Type SourceRegion) ->
  Parser (Expression SourceRegion) ->
  Parser (Expression SourceRegion)
pExpression pStmt pSimPat pPat pTy pExpr = do
  Megaparsec.choice
    [ pOp'
    , pWithoutOp
    ]
 where
  pWithoutOp =
    Megaparsec.choice
      [ pNotSuffixable
      , do
          head' <- pSuffixable
          Megaparsec.choice [pWithSuffix head', pure head']
      ]

  pWithSuffix expr = do
    Megaparsec.try do
      expr' <- pSuffix expr
      Megaparsec.choice [pWithSuffix expr', pure expr']

  pSuffixable =
    Megaparsec.choice
      [ pWildcard'
      , pLiteral'
      , Megaparsec.try pTuple'
      , pParens'
      , pVar'
      , pConstructor'
      , pMatch'
      , pBlock'
      ]

  pSuffix expr =
    Megaparsec.label "expression suffix" $
      Megaparsec.choice
        [ pOfTypeSuffix' expr
        , pIndexSuffix' expr
        , pDotAccessSuffix' expr
        , pAppSuffix' expr
        ]

  pNotSuffixable =
    Megaparsec.choice
      [ pWith'
      , pIf'
      , pLoop'
      , pLambda'
      -- TODO handle
      ]

  pWildcard' = do
    region <- pWildcard
    pure $ Expression EWildcard region

  pLiteral' = do
    (lit, region) <- pLiteral
    pure $ Expression (ELiteral lit) region

  pParens' = do
    (expr, region) <- pParens pExpr
    pure $ Expression (EParens expr) region

  pVar' = do
    var <- pVar pTy
    pure $ Expression (EVar var) var.ann

  pConstructor' = do
    cons <- pConstructor pTy
    pure $ Expression (EConstructor cons) cons.ann

  pOp' = pOperators pWithoutOp

  pWith' = do
    with <- pWith pStmt pTy pExpr
    pure $ Expression (EWithF with) with.ann

  pTuple' = pTuple pExpr

  pMatch' = do
    match <- pMatchExpression pPat pExpr
    pure $ Expression (EMatchF match) match.ann

  pIf' = do
    if' <- pIfExpression pStmt pPat pExpr
    pure $ Expression (EIfF if') if'.ann

  pLoop' = do
    loop <- pLoopExpression pStmt pExpr
    pure $ Expression (ELoopF loop) loop.ann

  pBlock' = do
    block <- pCodeBlock pStmt pExpr
    pure $ Expression (EBlockF block) block.ann

  pLambda' = do
    (lambda, ann) <- Megaparsec.choice [Megaparsec.try (pLambdaShort pTy pExpr), pLambdaFull pStmt pTy pExpr]
    pure $ Expression (ELambdaF lambda) ann

  pOfTypeSuffix' expr = do
    (expr', ty) <- pOfTypeSuffix pTy expr
    pure $
      Expression
        (EOfType expr' ty)
        SourceRegion{start = expr.ann.start, end = ty.ann.end}

  pIndexSuffix' expr = do
    (expr', exprIdx, ann) <- pIndexSuffix pExpr expr
    pure $ Expression (EIndex expr' exprIdx) ann

  pDotAccessSuffix' expr = do
    (expr', field, ann) <- pDotAccessSuffix pTy expr
    pure $ Expression (EDotAccess expr' field) ann

  pAppSuffix' expr = Megaparsec.label "application" do
    app <- pAppSuffix pTy pExpr expr
    pure $ Expression (EAppF app) app.ann
