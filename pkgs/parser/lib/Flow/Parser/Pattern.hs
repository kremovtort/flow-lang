module Flow.Parser.Pattern where

import Flow.AST.Surface qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (Parser)

pPattern :: Parser (Surface.Pattern Lexer.SourceRegion)
pPattern = fail "Flow.Parser.Pattern.pPattern: not implemented"


