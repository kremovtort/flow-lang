module Flow.Parser.Expr where

import "containers" Data.Set qualified as Set
import "megaparsec" Text.Megaparsec qualified as Megaparsec

import Flow.AST.Surface (Expression (..), Type (..))
import Flow.AST.Surface.Expr
import Flow.AST.Surface.Literal
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common

eWildcard :: Parser (Expression Lexer.SourceRegion)
eWildcard = do
  tok <- single (Lexer.Punctuation Lexer.Underscore)
  pure $ Expression{expr = EWildcard tok.payload, ann = tok.payload}

eLiteral :: Parser (Expression Lexer.SourceRegion)
eLiteral = do
  Megaparsec.choice [unit, bool, integer, float, byte, byteString, char, string]
 where
  unit = do
    tok <- single (Lexer.Punctuation Lexer.LeftRightParen)
    pure $ Expression{expr = ELiteral (LitUnit tok.payload) tok.payload, ann = tok.payload}
  bool = do
    tok <-
      token
        (Set.singleton $ Megaparsec.Label "bool literal")
        \case
            Lexer.BoolLiteral b -> Just b
            _ -> Nothing
    pure $
      Expression
        { expr = ELiteral (LitBool tok.token tok.payload) tok.payload
        , ann = tok.payload
        }

  integer = do
    tok <-
      token
        (Set.singleton $ Megaparsec.Label "integer literal")
        \case
            Lexer.IntegerLiteral i -> Just i
            _ -> Nothing
    pure $
      Expression
        { expr = ELiteral (LitInteger tok.token tok.payload) tok.payload
        , ann = tok.payload
        }

  float = do
    tok <-
      token
        (Set.singleton $ Megaparsec.Label "float literal")
        \case
            Lexer.FloatLiteral f -> Just f
            _ -> Nothing
    pure $
      Expression
        { expr = ELiteral (LitFloat tok.token tok.payload) tok.payload
        , ann = tok.payload
        }

  byte = do
    tok <-
      token
        (Set.singleton $ Megaparsec.Label "byte literal")
        \case
            Lexer.ByteLiteral b -> Just b
            _ -> Nothing
    pure $
      Expression
        { expr = ELiteral (LitByte tok.token tok.payload) tok.payload
        , ann = tok.payload
        }

  byteString = do
    tok <-
      token
        (Set.singleton $ Megaparsec.Label "byte string literal")
        \case
            Lexer.ByteStringLiteral b -> Just b
            _ -> Nothing
    pure $
      Expression
        { expr = ELiteral (LitByteString tok.token tok.payload) tok.payload
        , ann = tok.payload
        }

  char = do
    tok <-
      token
        (Set.singleton $ Megaparsec.Label "char literal")
        \case
            Lexer.CharLiteral c -> Just c
            _ -> Nothing
    pure $
      Expression
        { expr = ELiteral (LitChar tok.token tok.payload) tok.payload
        , ann = tok.payload
        }

  string = do
    tok <-
      token
        (Set.singleton $ Megaparsec.Label "string literal")
        \case
            Lexer.StringLiteral s -> Just s
            _ -> Nothing
    pure $
      Expression
        { expr = ELiteral (LitString tok.token tok.payload) tok.payload
        , ann = tok.payload
        }

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
      { expr =
          EParens
            expr'
            ann
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
      { expr =
          EOfType
            expr'
            ty'
            ann
      , ann
      }

-- | Top-level expression parser (stub for TDD phase)
pExpression :: Parser (Expression Lexer.SourceRegion)
pExpression = fail "Flow.Parser.Expr.pExpression: not implemented"
