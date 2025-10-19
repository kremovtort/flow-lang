module Flow.Parser.Literal where

import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "containers" Data.Set qualified as Set

import Flow.AST.Surface.Literal qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (Parser, token, single)

literal :: Parser (Surface.Literal, Lexer.SourceRegion)
literal = Megaparsec.choice [unit, bool, integer, float, byte, byteString, char, string]

unit :: Parser (Surface.Literal, Lexer.SourceRegion)
unit = do
  tok <- single (Lexer.Punctuation Lexer.LeftRightParen)
  pure (Surface.LitUnit, tok.payload)

bool :: Parser (Surface.Literal, Lexer.SourceRegion)
bool = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "bool literal")
      \case
          Lexer.BoolLiteral b -> Just b
          _ -> Nothing
  pure (Surface.LitBool tok.token, tok.payload)

integer :: Parser (Surface.Literal, Lexer.SourceRegion)
integer = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "integer literal")
      \case
          Lexer.IntegerLiteral i -> Just i
          _ -> Nothing
  pure (Surface.LitInteger tok.token, tok.payload)

float :: Parser (Surface.Literal, Lexer.SourceRegion)
float = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "float literal")
      \case
          Lexer.FloatLiteral f -> Just f
          _ -> Nothing
  pure (Surface.LitFloat tok.token, tok.payload)

byte :: Parser (Surface.Literal, Lexer.SourceRegion)
byte = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "byte literal")
      \case
          Lexer.ByteLiteral b -> Just b
          _ -> Nothing
  pure (Surface.LitByte tok.token, tok.payload)

byteString :: Parser (Surface.Literal, Lexer.SourceRegion)
byteString = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "byte string literal")
      \case
          Lexer.ByteStringLiteral b -> Just b
          _ -> Nothing
  pure (Surface.LitByteString tok.token, tok.payload)

char :: Parser (Surface.Literal, Lexer.SourceRegion)
char = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "char literal")
      \case
          Lexer.CharLiteral c -> Just c
          _ -> Nothing
  pure (Surface.LitChar tok.token, tok.payload)

string :: Parser (Surface.Literal, Lexer.SourceRegion)
string = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "string literal")
      \case
          Lexer.StringLiteral s -> Just s
          _ -> Nothing
  pure (Surface.LitString tok.token, tok.payload)
