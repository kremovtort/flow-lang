module Flow.Parser.Literal where

import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "containers" Data.Set qualified as Set

import Flow.AST.Surface.Literal qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (Parser, SourceRegion (..), token, single)

literal :: Parser (Surface.Literal, SourceRegion)
literal = Megaparsec.choice [unit, bool, integer, float, byte, byteString, char, string]

unit :: Parser (Surface.Literal, SourceRegion)
unit = do
  tok <- single (Lexer.Punctuation Lexer.LeftRightParen)
  pure (Surface.LitUnit, tok.region)

bool :: Parser (Surface.Literal, SourceRegion)
bool = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "bool literal")
      \case
          Lexer.BoolLiteral b -> Just b
          _ -> Nothing
  pure (Surface.LitBool tok.value, tok.region)

integer :: Parser (Surface.Literal, Lexer.SourceRegion)
integer = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "integer literal")
      \case
          Lexer.IntegerLiteral i -> Just i
          _ -> Nothing
  pure (Surface.LitInteger tok.value, tok.region)

float :: Parser (Surface.Literal, Lexer.SourceRegion)
float = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "float literal")
      \case
          Lexer.FloatLiteral f -> Just f
          _ -> Nothing
  pure (Surface.LitFloat tok.value, tok.region)

byte :: Parser (Surface.Literal, Lexer.SourceRegion)
byte = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "byte literal")
      \case
          Lexer.ByteLiteral b -> Just b
          _ -> Nothing
  pure (Surface.LitByte tok.value, tok.region)

byteString :: Parser (Surface.Literal, Lexer.SourceRegion)
byteString = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "byte string literal")
      \case
          Lexer.ByteStringLiteral b -> Just b
          _ -> Nothing
  pure (Surface.LitByteString tok.value, tok.region)

char :: Parser (Surface.Literal, Lexer.SourceRegion)
char = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "char literal")
      \case
          Lexer.CharLiteral c -> Just c
          _ -> Nothing
  pure (Surface.LitChar tok.value, tok.region)

string :: Parser (Surface.Literal, Lexer.SourceRegion)
string = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "string literal")
      \case
          Lexer.StringLiteral s -> Just s
          _ -> Nothing
  pure (Surface.LitString tok.value, tok.region)
