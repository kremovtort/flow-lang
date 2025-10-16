{-# LANGUAGE DuplicateRecordFields #-}
module Flow.Parser where

import "text" Data.Text (Text)

import Flow.AST.Surface qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common qualified as P
import Flow.Parser.Type qualified as PType
import Flow.Parser.Expr qualified as PExpr
import Flow.Parser.Syntax qualified as PSyn
import Flow.Parser.Pattern qualified as PPat
import Flow.Parser.Module qualified as PMod
import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "megaparsec" Text.Megaparsec (parse)
import "text" Data.Text qualified as Text
import "vector" Data.Vector qualified as Vector
import "base" Data.Foldable (toList)

-- | Temporary public API stubs for TDD phase.
-- Tests will call these; implementations will be provided later.

parseModText :: Text -> Either String (Surface.Mod Lexer.SourceRegion)
parseModText txt =
  case textToTokens txt of
    Left err -> Left err
    Right toks ->
      case parse PMod.pModule "<mod>" toks of
        Left e -> Left (show e)
        Right r -> Right r

parseTypeText :: Text -> Either String (Surface.Type Lexer.SourceRegion)
parseTypeText txt =
  case textToTokens txt of
    Left err -> Left err
    Right toks ->
      case parse PType.pType "<type>" toks of
        Left e -> Left (show e)
        Right r -> Right r

parsePatternText :: Text -> Either String (Surface.Pattern Lexer.SourceRegion)
parsePatternText txt =
  case textToTokens txt of
    Left err -> Left err
    Right toks ->
      case parse PPat.pPattern "<pattern>" toks of
        Left e -> Left (show e)
        Right r -> Right r

parseLhsExprText :: Text -> Either String (Surface.LHSExpression Lexer.SourceRegion)
parseLhsExprText txt =
  case textToTokens txt of
    Left err -> Left err
    Right toks ->
      case parse PSyn.pLHSExpression "<lhs-expr>" toks of
        Left e -> Left (show e)
        Right r -> Right r

parseExprText :: Text -> Either String (Surface.Expression Lexer.SourceRegion)
parseExprText txt =
  case textToTokens txt of
    Left err -> Left err
    Right toks ->
      case parse PExpr.pExpression "<expr>" toks of
        Left e -> Left (show e)
        Right r -> Right r

-- Helpers

textToTokens :: Text -> Either String [Lexer.TokenWithSourceRegion]
textToTokens t =
  case Megaparsec.runParser Lexer.tokensWithSourceRegion "<lex>" t of
    Left e -> Left (show e)
    Right v -> Right (toList v)
