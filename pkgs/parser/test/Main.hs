module Main (main) where

import "hspec" Test.Hspec (hspec)

import Flow.LexerSpec qualified

main :: IO ()
main = hspec do
  Flow.LexerSpec.spec
