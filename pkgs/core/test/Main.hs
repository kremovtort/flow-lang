module Main where

import "hspec" Test.Hspec (hspec)

import Flow.Core.ModuleSpec qualified as ModuleSpec

main :: IO ()
main = hspec do
  ModuleSpec.spec
