module Flow.Parser.With where

import "base" Data.Maybe (fromJust, listToMaybe)
import "megaparsec" Text.Megaparsec (SourcePos)
import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector

import Data.List.NonEmpty qualified as NonEmptyList
import Flow.AST.Ann (SourceSpan (..))
import Flow.AST.Surface.Common (SimpleVarIdentifier (..))
import Flow.AST.Surface.Syntax (CodeBlockF (..))
import Flow.AST.Surface.With (
  InStatementF (..),
  WithAppF (..),
  WithAppFieldsF (..),
  WithAppNamedClauseF (..),
  WithBlockF (..),
  WithLhsF (..),
  WithRhsExprF (..),
  WithRhsF (..),
  WithStatementF (..),
 )
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (HasAnn, Parser, pSimpleVarIdentifier, single)
import Flow.Parser.Syntax (pCodeBlock)

pWithApp ::
  (HasAnn ty SourceSpan, HasAnn expr SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (WithAppF ty expr SourceSpan)
pWithApp pTy pExpr = Megaparsec.label "with in function call" do
  tokS <- single (Lexer.Keyword Lexer.With)
  (fields, end) <- pWithAppFields
  pure $
    WithAppF
      { fields
      , ann = SourceSpan{start = tokS.span.start, end}
      }
 where
  pWithAppFields =
    Megaparsec.choice
      [ pWithAppUnnamed
      , pWithAppNamed
      ]

  pWithAppUnnamed =
    do
      _ <- single (Lexer.Punctuation Lexer.LeftParen)
      rhs <-
        NonEmptyList.fromList
          <$> Megaparsec.sepEndBy1
            (pWithRhs pTy pExpr)
            (single (Lexer.Punctuation Lexer.Comma))
      tokE <- single (Lexer.Punctuation Lexer.RightParen)
      pure
        ( WAppUnnamedF $ NonEmptyVector.fromNonEmpty $ fmap fst rhs
        , tokE.span.end
        )

  pWithAppNamed =
    do
      _ <- single (Lexer.Punctuation Lexer.LeftBrace)
      clauses <-
        NonEmptyList.fromList
          <$> Megaparsec.sepEndBy1
            pWithAppNamedClause
            (single (Lexer.Punctuation Lexer.Comma))
      tokE <- single (Lexer.Punctuation Lexer.RightBrace)
      pure
        ( WAppNamedF $ NonEmptyVector.fromNonEmpty clauses
        , tokE.span.end
        )

  pWithAppNamedClause = do
    lhs <-
      NonEmptyList.fromList
        <$> Megaparsec.sepEndBy1 (pWithLhs pTy) (single (Lexer.Punctuation Lexer.Comma))
    _ <- single (Lexer.Punctuation Lexer.Assign)
    rhs <- pWithRhs pTy pExpr
    pure $
      WithAppNamedClauseF
        { lhs = NonEmptyVector.fromNonEmpty $ fmap fst lhs
        , rhs = fst rhs
        , ann =
            SourceSpan
              { start = snd $ NonEmptyList.last lhs
              , end = snd rhs
              }
        }

pWithBlock ::
  (HasAnn stmt SourceSpan, HasAnn ty SourceSpan, HasAnn expr SourceSpan) =>
  Parser (stmt SourceSpan) ->
  Parser (ty SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (WithBlockF stmt ty expr SourceSpan)
pWithBlock pStmt pTy pExpr = do
  tokS <- single (Lexer.Keyword Lexer.With)
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  statements <-
    Megaparsec.sepEndBy1
      (pWithStatement pTy pExpr)
      (single (Lexer.Punctuation Lexer.Semicolon))
  _ <- single (Lexer.Punctuation Lexer.RightBrace)
  _ <- single (Lexer.Keyword Lexer.In)
  block <- pCodeBlock pStmt pExpr
  pure
    WithBlockF
      { withStatements = fromJust $ NonEmptyVector.fromList statements
      , block
      , ann = SourceSpan{start = tokS.span.start, end = block.ann.end}
      }

pWithStatement ::
  (HasAnn ty SourceSpan, HasAnn expr SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (WithStatementF ty expr SourceSpan)
pWithStatement pTy pExpr = do
  let_ <- Megaparsec.optional (single (Lexer.Keyword Lexer.Let))
  lhs <- Megaparsec.sepEndBy1 (pWithLhs pTy) (single (Lexer.Punctuation Lexer.Comma))
  let start = case let_ of
        Just let_' -> let_'.span.start
        Nothing -> snd $ fromJust $ listToMaybe lhs
  _ <- single (Lexer.Punctuation Lexer.Assign)
  (rhs, end) <- pWithRhs pTy pExpr
  pure $
    WithStatementF
      { let_ = fmap (.span) let_
      , lhs = fromJust $ NonEmptyVector.fromList $ map fst lhs
      , rhs = rhs
      , ann = SourceSpan{start = start, end = end}
      }

pWithLhs ::
  (HasAnn ty SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (WithLhsF ty SourceSpan, SourcePos)
pWithLhs pTy = do
  Megaparsec.choice
    [ pWithLhsLabelled
    , pWithLhsUnlabelled
    ]
 where
  pWithLhsLabelled = do
    name <- pSimpleVarIdentifier
    ty <- Megaparsec.optional do
      _ <- single (Lexer.Punctuation Lexer.Colon)
      pTy
    pure
      ( WLhsLabelledF name ty
      , name.ann.start
      )
  pWithLhsUnlabelled = do
    ty <- pTy
    pure (WLhsUnlabelledF ty, ty.ann.start)

pWithRhs ::
  (HasAnn ty SourceSpan, HasAnn expr SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (WithRhsF ty expr SourceSpan, SourcePos)
pWithRhs pTy pExpr =
  Megaparsec.choice
    [ Megaparsec.try pWithRhsExpr
    , pWithRhsType
    ]
 where
  pWithRhsExpr = do
    expr' <- pExpr
    in_ <- Megaparsec.optional do
      _ <- single (Lexer.Keyword Lexer.In)
      _ <- single (Lexer.Punctuation Lexer.LeftBrace)
      statements <-
        Megaparsec.sepEndBy1
          (pWithRhsInStatement pTy pExpr)
          (single (Lexer.Punctuation Lexer.Comma))
      _ <- single (Lexer.Punctuation Lexer.RightBrace)
      pure $ fromJust $ NonEmptyVector.fromList statements
    pure
      ( WRhsExprF
          ( WithRhsExprF
              { expr = expr'
              , in_
              , ann = expr'.ann
              }
          )
      , expr'.ann.end
      )

  pWithRhsType = do
    ty <- pTy
    pure (WRhsTypeF ty, ty.ann.end)

pWithRhsInStatement ::
  (HasAnn ty SourceSpan, HasAnn expr SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (InStatementF ty expr SourceSpan)
pWithRhsInStatement pTy pExpr = do
  lhs <- fromJust . NonEmptyVector.fromList <$> Megaparsec.sepEndBy1 pTy (single (Lexer.Punctuation Lexer.Comma))
  _ <- single (Lexer.Punctuation Lexer.Assign)
  (rhs, end) <- pWithRhs pTy pExpr
  pure $
    InStatementF
      { lhs
      , rhs
      , ann = SourceSpan{start = (NonEmptyVector.head lhs).ann.start, end}
      }
