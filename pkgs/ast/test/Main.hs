module Main (main) where

import "hspec" Test.Hspec (hspec)

import Flow.ASTSpec qualified

main :: IO ()
main = hspec do
  Flow.ASTSpec.spec

