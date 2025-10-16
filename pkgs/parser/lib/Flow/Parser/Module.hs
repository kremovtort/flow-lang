module Flow.Parser.Module where

import Flow.AST.Surface qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (Parser)

pModule :: Parser (Surface.Mod Lexer.SourceRegion)
pModule = fail "Flow.Parser.Module.pModule: not implemented"


