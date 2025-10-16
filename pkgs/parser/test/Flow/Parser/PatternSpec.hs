module Flow.Parser.PatternSpec (spec) where

import "hspec" Test.Hspec (Spec, describe, it)

import Flow.Parser.Helpers (testParser)
import Flow.Parser.Pattern qualified as PPat

spec :: Spec
spec = describe "Pattern parser (minimal subset)" do
  it "parses wildcard _" do
    testParser "_" PPat.pPattern Nothing

  it "parses literal true" do
    testParser "true" PPat.pPattern Nothing

  it "parses variable x" do
    testParser "x" PPat.pPattern Nothing

  it "parses tuple (x, y)" do
    testParser "(x, y)" PPat.pPattern Nothing

  it "parses constructor Some(1)" do
    testParser "Some(1)" PPat.pPattern Nothing

  it "parses constructor with named fields Cons { head = 1, tail = xs }" do
    testParser "Cons { head = 1, tail = xs }" PPat.pPattern Nothing


