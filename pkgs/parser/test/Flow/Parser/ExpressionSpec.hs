module Flow.Parser.ExpressionSpec (spec) where

import "hspec" Test.Hspec (Spec, describe, it)

import Flow.Parser.Helpers (testParser)
import Flow.Parser.Expr qualified as PExpr

spec :: Spec
spec = describe @() "Expression parser (minimal subset)" do
  it "parses wildcard _" do
    testParser "_" PExpr.pExpression Nothing

  it "parses literal 1" do
    testParser "1" PExpr.pExpression Nothing

  it "parses variable x" do
    testParser "x" PExpr.pExpression Nothing

  it "parses parens (x)" do
    testParser "(x)" PExpr.pExpression Nothing

  it "parses unary &x, &mut x, &'s x, -x, !x" do
    mapM_ (\txt -> testParser txt PExpr.pExpression Nothing)
      ["&x", "&mut x", "&'s x", "-x", "!x"]

  it "parses binary 1 + 2 * 3" do
    testParser "1 + 2 * 3" PExpr.pExpression Nothing

  it "parses calls f(a, b) and with named args f(x = 1, y = 2)" do
    mapM_ (\txt -> testParser txt PExpr.pExpression Nothing)
      ["f(a, b)", "f(x = 1, y = 2)"]

  it "parses call with type/scope params f<'s, T>(a)" do
    testParser "f<'s, T>(a)" PExpr.pExpression Nothing

  it "parses chained access a.b[0]" do
    testParser "a.b[0]" PExpr.pExpression Nothing

  it "parses tuple (a, b)" do
    testParser "(a, b)" PExpr.pExpression Nothing

  it "parses simple block { let x = 1; x }" do
    testParser "{ let x = 1; x }" PExpr.pExpression Nothing


