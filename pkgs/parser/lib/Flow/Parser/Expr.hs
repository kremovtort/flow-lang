module Flow.Parser.Expr where

import "containers" Data.Set qualified as Set
import "megaparsec" Text.Megaparsec qualified as Megaparsec

import Flow.AST.Surface (Expression (..), Type (..))
import Flow.AST.Surface.Expr
    ( ExpressionF(..) )
import Flow.Parser.Literal (literal)
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common ( Parser, single )

eWildcard :: Parser (Expression Lexer.SourceRegion)
eWildcard = do
  tok <- single (Lexer.Punctuation Lexer.Underscore)
  pure $ Expression{expr = EWildcard, ann = tok.payload}

eLiteral :: Parser (Expression Lexer.SourceRegion)
eLiteral = do
  (lit, ann) <- literal
  pure $ Expression{expr = ELiteral lit, ann}

eParens ::
  Parser (Expression Lexer.SourceRegion) ->
  Parser (Expression Lexer.SourceRegion)
eParens expr = do
  tokS <- single (Lexer.Punctuation Lexer.LeftParen)
  expr' <- expr
  tokE <- single (Lexer.Punctuation Lexer.RightParen)
  let ann = Lexer.SourceRegion{start = tokS.payload.start, end = tokE.payload.end}
  pure $
    Expression
      { expr = EParens expr'
      , ann
      }

eOfType ::
  Parser (Expression Lexer.SourceRegion) ->
  Parser (Type Lexer.SourceRegion) ->
  Parser (Expression Lexer.SourceRegion)
eOfType expr ty = do
  expr' <- expr
  ty' <- ty
  let ann = Lexer.SourceRegion{start = expr'.ann.start, end = ty'.ann.end}
  pure $
    Expression
      { expr = EOfType expr' ty'
      , ann
      }

-- | Top-level expression parser (stub for TDD phase)
pExpression :: Parser (Expression Lexer.SourceRegion)
pExpression = fail "Flow.Parser.Expr.pExpression: not implemented"
