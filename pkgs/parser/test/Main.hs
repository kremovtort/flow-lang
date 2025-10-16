module Main (main) where

import "hspec" Test.Hspec (hspec)

import Flow.LexerSpec qualified
import Flow.Parser.TypeSpec qualified
import Flow.Parser.PatternSpec qualified
import Flow.Parser.LHSExpressionSpec qualified
import Flow.Parser.ExpressionSpec qualified
import Flow.Parser.ModuleSpec qualified

main :: IO ()
main = hspec do
  Flow.LexerSpec.spec
  Flow.Parser.TypeSpec.spec
  Flow.Parser.PatternSpec.spec
  Flow.Parser.LHSExpressionSpec.spec
  Flow.Parser.ExpressionSpec.spec
  Flow.Parser.ModuleSpec.spec
