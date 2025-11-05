module Flow.Parser.Syntax where

import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector
import "vector" Data.Vector qualified as Vector

import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Flow.AST.Surface (
  Expression (..),
  LHSExpression (..),
 )
import Flow.AST.Surface.Common (SimpleVarIdentifier (..))
import Flow.AST.Surface.Syntax
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (
  HasAnn,
  Parser,
  SourceRegion (..),
  WithPos (region),
  scopeIdentifier,
  simpleVarIdentifier,
  single,
  token,
 )
import Flow.Parser.Use (pUseClause)

pCodeBlock ::
  (HasAnn stmt SourceRegion, HasAnn expr SourceRegion) =>
  Parser (stmt SourceRegion) ->
  Parser (expr SourceRegion) ->
  Parser (CodeBlockF stmt expr SourceRegion)
pCodeBlock pStmt pExpr = do
  tokS <- single (Lexer.Punctuation Lexer.LeftBrace)
  region <- Megaparsec.optional do
    region <- scopeIdentifier
    _ <- single (Lexer.Punctuation Lexer.FatArrow)
    pure region
  uses <- Megaparsec.many pUseClause
  statements <- Megaparsec.many pStmt
  result <- Megaparsec.optional pExpr
  tokE <- single (Lexer.Punctuation Lexer.RightBrace)
  let ann = SourceRegion{start = tokS.region.start, end = tokE.region.end}
  pure $
    CodeBlockF
      { region
      , uses = Vector.fromList uses
      , statements = Vector.fromList statements
      , result
      , ann
      }

pStatement ::
  ( HasAnn stmt SourceRegion
  , HasAnn lhs SourceRegion
  , HasAnn simPat SourceRegion
  , HasAnn pat SourceRegion
  , HasAnn ty SourceRegion
  , HasAnn expr SourceRegion
  ) =>
  Parser (stmt SourceRegion) ->
  Parser (lhs SourceRegion) ->
  Parser (simPat SourceRegion) ->
  Parser (pat SourceRegion) ->
  Parser (ty SourceRegion) ->
  Parser (expr SourceRegion) ->
  Parser (StatementF stmt lhs simPat pat ty expr SourceRegion, SourceRegion)
pStatement pStmt pLhsExpr pSimPat pPat pTy pExpr = do
  Megaparsec.choice
    [ letStatement
    , returnStatement
    , continueStatement
    , breakStatement
    , matchStatement
    , ifStatement
    , loopStatement
    , whileStatement
    , forStatement
    , Megaparsec.try assignStatement
    , Megaparsec.try expressionStatement
    ]
 where
  letStatement = do
    let' <- pLetDefinition pSimPat pTy pExpr
    pure
      ( SLetF let'
      , let'.ann
      )

  assignStatement = do
    lhs <- pLhsExpr
    _ <- single (Lexer.Punctuation Lexer.Assign)
    rhs <- pExpr
    semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
    let ann = SourceRegion{start = lhs.ann.start, end = semicolonTok.region.end}
    pure
      ( SAssignF $
          AssignStatementF
            { lhs = lhs
            , rhs = rhs
            , ann = ann
            }
      , ann
      )

  returnStatement = do
    returnTok <- single (Lexer.Keyword Lexer.Return)
    expr <- pExpr
    semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
    let ann = SourceRegion{start = returnTok.region.start, end = semicolonTok.region.end}
    pure (SReturnF expr ann, ann)

  continueStatement = do
    continueTok <- single (Lexer.Keyword Lexer.Continue)
    label <- Megaparsec.optional simpleVarIdentifier
    semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
    let ann = SourceRegion{start = continueTok.region.start, end = semicolonTok.region.end}
    pure (SContinueF label ann, ann)

  breakStatement = do
    breakTok <- single (Lexer.Keyword Lexer.Break)
    label <- Megaparsec.optional simpleVarIdentifier
    semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
    let ann = SourceRegion{start = breakTok.region.start, end = semicolonTok.region.end}
    pure (SBreakF label ann, ann)

  expressionStatement = do
    expr <- pExpr
    _ <- single (Lexer.Punctuation Lexer.Semicolon)
    pure (SExpressionF expr, expr.ann)

  matchStatement = do
    expr <- pMatchExpression pPat pExpr
    pure (SMatchF expr, expr.ann)

  ifStatement = do
    expr' <- pIfExpression pStmt pPat pExpr
    pure (SIfF expr', expr'.ann)

  loopStatement = do
    expr' <- pLoopExpression pStmt pExpr
    pure (SLoopF expr', expr'.ann)

  whileStatement = do
    expr' <- pWhileStatement pStmt pPat pExpr
    pure (SWhileF expr', expr'.ann)

  forStatement = do
    expr' <- pForStatement pSimPat pExpr
    pure (SForF expr', expr'.ann)

pLetDefinition ::
  (HasAnn simPat SourceRegion, HasAnn ty SourceRegion, HasAnn expr SourceRegion) =>
  Parser (simPat SourceRegion) ->
  Parser (ty SourceRegion) ->
  Parser (expr SourceRegion) ->
  Parser (LetDefinitionF simPat ty expr SourceRegion)
pLetDefinition pSimPat pTy pExpr = do
  letTok <- single (Lexer.Keyword Lexer.Let)
  lhs <- pSimPat
  lhsType <- Megaparsec.optional do
    _ <- single (Lexer.Punctuation Lexer.Colon)
    pTy
  _ <- single (Lexer.Punctuation Lexer.Assign)
  rhs <- pExpr
  semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
  let ann = SourceRegion{start = letTok.region.start, end = semicolonTok.region.end}
  pure
    LetDefinitionF
      { lhs = lhs
      , lhsType = lhsType
      , rhs = rhs
      , ann = ann
      }

-- | LHSExpression parser (stub)
pLHSExpression :: Parser (Expression SourceRegion) -> Parser (LHSExpression SourceRegion)
pLHSExpression expr = do
  base <- pLHSEAtom expr
  suffixes base
 where
  suffixes acc = do
    mSuffix <- Megaparsec.optional (Megaparsec.choice [pLHSEIndexSuffix expr, pLHSEDotAccessSuffix])
    case mSuffix of
      Just applySuffix -> suffixes (applySuffix acc)
      Nothing -> pure acc

-- | Parses an index suffix: "[expr]" and returns a function to extend LHS
pLHSEIndexSuffix :: Parser (Expression SourceRegion) -> Parser (LHSExpression SourceRegion -> LHSExpression SourceRegion)
pLHSEIndexSuffix expr = do
  _ <- single (Lexer.Punctuation Lexer.LeftBracket)
  idxExpr <- expr
  tokE <- single (Lexer.Punctuation Lexer.RightBracket)
  pure $ \acc ->
    LHSExpression
      { lhsExpression = LHSEIndex acc idxExpr
      , ann = SourceRegion{start = acc.ann.start, end = tokE.region.end}
      }

-- | Parses a dot-access suffix: ".field" and returns a function to extend LHS
pLHSEDotAccessSuffix :: Parser (LHSExpression SourceRegion -> LHSExpression SourceRegion)
pLHSEDotAccessSuffix = do
  _ <- single (Lexer.Punctuation Lexer.Dot)
  field' <- simpleVarIdentifier
  pure $ \acc ->
    LHSExpression
      { lhsExpression = LHSEDotAccess acc field'
      , ann = SourceRegion{start = acc.ann.start, end = field'.ann.end}
      }

pLHSEAtom :: Parser (Expression SourceRegion) -> Parser (LHSExpression SourceRegion)
pLHSEAtom expr = do
  Megaparsec.choice
    [ pLHSEWildcard
    , pLHSEVar
    , pLHSEUnOp expr
    ]

pLHSEWildcard :: Parser (LHSExpression SourceRegion)
pLHSEWildcard = do
  tok <- single (Lexer.Punctuation Lexer.Underscore)
  pure $ LHSExpression{lhsExpression = LHSEWildcard, ann = tok.region}

pLHSEVar :: Parser (LHSExpression SourceRegion)
pLHSEVar = do
  var <- simpleVarIdentifier
  pure $ LHSExpression{lhsExpression = LHSEVar var, ann = var.ann}

pLHSEUnOp :: Parser (Expression SourceRegion) -> Parser (LHSExpression SourceRegion)
pLHSEUnOp pExpr = do
  tokS <- single (Lexer.Punctuation Lexer.Star)
  expr <- pExpr
  pure $ LHSExpression{lhsExpression = LHSEUnOp (LHSUnOpExpressionDeref expr), ann = SourceRegion{start = tokS.region.start, end = expr.ann.end}}

pMatchExpression ::
  (HasAnn pat SourceRegion, HasAnn expr SourceRegion) =>
  Parser (pat SourceRegion) ->
  Parser (expr SourceRegion) ->
  Parser (MatchExpressionF pat expr SourceRegion)
pMatchExpression pPat pExpr = do
  matchTok <- single (Lexer.Keyword Lexer.Match)
  value <- pExpr
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  arms <- Megaparsec.sepEndBy1 matchArm (single (Lexer.Punctuation Lexer.Comma))
  rightBraceTok <- single (Lexer.Punctuation Lexer.RightBrace)
  let ann = SourceRegion{start = matchTok.region.start, end = rightBraceTok.region.end}
  pure $
    MatchExpressionF
      { value = value
      , arms = fromJust $ NonEmptyVector.fromList arms
      , ann = ann
      }
 where
  matchArm = do
    pattern' <- pPat
    guard <- Megaparsec.optional do
      _ <- single (Lexer.Keyword Lexer.If)
      pExpr
    _ <- single (Lexer.Punctuation Lexer.FatArrow)
    expr' <- pExpr
    pure $
      MatchArmF
        { pattern = pattern'
        , guard = guard
        , expression = expr'
        , ann = pattern'.ann
        }

pIfExpression ::
  ( HasAnn stmt SourceRegion
  , HasAnn pat SourceRegion
  , HasAnn expr SourceRegion
  ) =>
  Parser (stmt SourceRegion) ->
  Parser (pat SourceRegion) ->
  Parser (expr SourceRegion) ->
  Parser (IfExpressionF stmt pat expr SourceRegion)
pIfExpression pStmt pPat pExpr = do
  branches' <- Megaparsec.sepBy1 pIfBranch (single (Lexer.Keyword Lexer.Else))
  let branches = fromJust $ NonEmptyVector.fromList branches'
  else_ <- Megaparsec.optional do
    _ <- single (Lexer.Keyword Lexer.Else)
    pCodeBlock pStmt pExpr
  let ann =
        SourceRegion
          { start = (NonEmptyVector.head branches).ann.start
          , end = case else_ of
              Just else' -> else'.ann.end
              Nothing -> (NonEmptyVector.last branches).ann.end
          }
  pure $
    IfExpressionF
      { branches
      , else_
      , ann
      }
 where
  pIfBranch = do
    ifTok <- single (Lexer.Keyword Lexer.If)
    (condition, conditionAnn) <- pCondition pPat pExpr
    result <- pCodeBlock pStmt pExpr
    pure
      IfBranchF
        { condition
        , result
        , ann = SourceRegion{start = ifTok.region.start, end = conditionAnn.end}
        }

pCondition ::
  (HasAnn pat SourceRegion, HasAnn expr SourceRegion) =>
  Parser (pat SourceRegion) ->
  Parser (expr SourceRegion) ->
  Parser (ConditionF pat expr SourceRegion, SourceRegion)
pCondition pPat pExpr =
  Megaparsec.choice
    [ pCondLet
    , pCondBool
    ]
 where
  pCondLet = do
    tokS <- single (Lexer.Keyword Lexer.Let)
    pat <- pPat
    _ <- single (Lexer.Punctuation Lexer.Assign)
    expr <- pExpr
    bool <- Megaparsec.optional do
      _ <- single (Lexer.Punctuation Lexer.Semicolon)
      pExpr
    let ann = case bool of
          Nothing -> SourceRegion tokS.region.start expr.ann.end
          Just b -> SourceRegion tokS.region.start b.ann.end
    pure
      ( CondLetF
          LetConditionF
            { pattern = pat
            , patternExpr = expr
            , bool
            , ann
            }
      , ann
      )

  pCondBool = pExpr <&> \expr -> (CondBoolF expr, expr.ann)

pLoopExpression ::
  (HasAnn stmt SourceRegion, HasAnn expr SourceRegion) =>
  Parser (stmt SourceRegion) ->
  Parser (expr SourceRegion) ->
  Parser (LoopExpressionF stmt expr SourceRegion)
pLoopExpression pStmt pExpr = do
  label <- Megaparsec.optional do
    labelTok <- token
      (Set.singleton $ Megaparsec.Label "label")
      \case
        Lexer.RefScope s -> Just s
        _ -> Nothing
    _ <- single (Lexer.Punctuation Lexer.Colon)
    pure
      ( SimpleVarIdentifier
          { name = labelTok.value
          , ann = labelTok.region
          }
      , SourceRegion
          { start = labelTok.region.start
          , end = labelTok.region.end
          }
      )
  loopTok <- Megaparsec.try $ single (Lexer.Keyword Lexer.Loop)
  body <- pCodeBlock pStmt pExpr
  let ann =
        SourceRegion
          { start = case label of
              Just (_, ann') -> ann'.start
              Nothing -> loopTok.region.start
          , end = body.ann.end
          }
  pure $
    LoopExpressionF
      { label = fmap fst label
      , body
      , ann
      }

pWhileStatement ::
  (HasAnn stmt SourceRegion, HasAnn pat SourceRegion, HasAnn expr SourceRegion) =>
  Parser (stmt SourceRegion) ->
  Parser (pat SourceRegion) ->
  Parser (expr SourceRegion) ->
  Parser (WhileStatementF stmt pat expr SourceRegion)
pWhileStatement pStmt pPat pExpr = do
  label <- Megaparsec.optional do
    labelTok <- token
      (Set.singleton $ Megaparsec.Label "label")
      \case
        Lexer.RefScope s -> Just s
        _ -> Nothing
    _ <- single (Lexer.Punctuation Lexer.Colon)
    pure
      ( SimpleVarIdentifier labelTok.value labelTok.region
      , SourceRegion
          { start = labelTok.region.start
          , end = labelTok.region.end
          }
      )
  whileTok <- Megaparsec.try $ single (Lexer.Keyword Lexer.While)
  (condition, _) <- pCondition pPat pExpr
  body <- pCodeBlock pStmt pExpr
  let ann =
        SourceRegion
          { start = case label of
              Just (_, ann') -> ann'.start
              Nothing -> whileTok.region.start
          , end = body.ann.end
          }
  pure $
    WhileStatementF
      { label = fmap fst label
      , condition
      , body
      , ann
      }

pForStatement ::
  (HasAnn simPat SourceRegion, HasAnn expr SourceRegion) =>
  Parser (simPat SourceRegion) ->
  Parser (expr SourceRegion) ->
  Parser (ForStatementF simPat expr SourceRegion)
pForStatement pSimPat pExpr = do
  label <- Megaparsec.optional do
    labelTok <- token
      (Set.singleton $ Megaparsec.Label "label")
      \case
        Lexer.RefScope s -> Just s
        _ -> Nothing
    _ <- single (Lexer.Punctuation Lexer.Colon)
    pure
      ( SimpleVarIdentifier labelTok.value labelTok.region
      , SourceRegion
          { start = labelTok.region.start
          , end = labelTok.region.end
          }
      )
  forTok <- Megaparsec.try $ single (Lexer.Keyword Lexer.For)
  pattern <- pSimPat
  _ <- single (Lexer.Keyword Lexer.In)
  iterable <- pExpr
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  body <- pExpr
  rightBraceTok <- single (Lexer.Punctuation Lexer.RightBrace)
  let ann =
        SourceRegion
          { start = case label of
              Just (_, ann') -> ann'.start
              Nothing -> forTok.region.start
          , end = rightBraceTok.region.end
          }
  pure $
    ForStatementF
      { label = label
      , pattern = pattern
      , iterable = iterable
      , body = body
      , ann = ann
      }
