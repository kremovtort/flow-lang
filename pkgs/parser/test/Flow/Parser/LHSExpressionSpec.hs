module Flow.Parser.LHSExpressionSpec (spec) where

import "hspec" Test.Hspec (Spec, describe, it)

import Flow.Parser.Helpers (testParser)
import Flow.Parser.Syntax qualified as PSyn

spec :: Spec
spec = describe "LHSExpression parser (minimal subset)" do
  it "parses variable x" do
    testParser "x" PSyn.pLHSExpression Nothing

  it "parses index a[b]" do
    testParser "a[b]" PSyn.pLHSExpression Nothing

  it "parses dot access a.b" do
    testParser "a.b" PSyn.pLHSExpression Nothing

  it "parses deref *a" do
    testParser "*a" PSyn.pLHSExpression Nothing
