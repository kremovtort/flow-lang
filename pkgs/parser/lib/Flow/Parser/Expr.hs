module Flow.Parser.Expr where

import Flow.AST.Surface (Expression (..), Type (..))
import Flow.AST.Surface.Constraint (AnyVarIdentifier (..))
import Flow.AST.Surface.Expr (
  ExpressionF (..),
 )
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (
  Parser,
  SourceRegion (..),
  WithPos (..),
  single,
 )
import Flow.Parser.Constraint (anyVarIdentifier)
import Flow.Parser.Literal (literal)

pWildcard :: Parser (Expression Lexer.SourceRegion)
pWildcard = do
  tok <- single (Lexer.Punctuation Lexer.Underscore)
  pure $ Expression{expr = EWildcard, ann = tok.region}

pLiteral :: Parser (Expression Lexer.SourceRegion)
pLiteral = do
  (lit, ann) <- literal
  pure $ Expression{expr = ELiteral lit, ann}

pParens ::
  Parser (Expression Lexer.SourceRegion) ->
  Parser (Expression Lexer.SourceRegion)
pParens expr = do
  tokS <- single (Lexer.Punctuation Lexer.LeftParen)
  expr' <- expr
  tokE <- single (Lexer.Punctuation Lexer.RightParen)
  pure $
    Expression
      { expr = EParens expr'
      , ann = Lexer.SourceRegion{start = tokS.region.start, end = tokE.region.end}
      }

pVar ::
  Parser (Type Lexer.SourceRegion) ->
  Parser (Expression Lexer.SourceRegion)
pVar pTy = do
  var <- anyVarIdentifier pTy
  pure $ Expression{expr = EVar var, ann = var.ann}

eOfType ::
  Parser (Expression SourceRegion) ->
  Parser (Type SourceRegion) ->
  Parser (Expression SourceRegion)
eOfType expr ty = do
  expr' <- expr
  ty' <- ty
  pure $
    Expression
      { expr = EOfType expr' ty'
      , ann = SourceRegion{start = expr'.ann.start, end = ty'.ann.end}
      }

-- | Top-level expression parser (stub for TDD phase)
pExpression :: Parser (Expression Lexer.SourceRegion)
pExpression = fail "Flow.Parser.Expr.pExpression: not implemented"
