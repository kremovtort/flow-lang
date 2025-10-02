module Main (main) where

import "hspec" Test.Hspec (hspec)

import Flow.Parser.LexerSpec qualified

main :: IO ()
main = hspec do
  Flow.Parser.LexerSpec.spec
