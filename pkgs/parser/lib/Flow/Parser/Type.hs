module Flow.Parser.Type where

import "megaparsec" Text.Megaparsec (MonadParsec)

import Flow.AST.Surface qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (Parser)

pType :: Parser (Surface.Type Lexer.SourceRegion)
pType = fail "Flow.Parser.Type.pType: not implemented"


