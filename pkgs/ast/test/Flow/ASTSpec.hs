module Flow.ASTSpec (spec) where

import "hspec" Test.Hspec (Spec, describe, it, shouldBe)

import "flow-ast" Flow.AST

spec :: Spec
spec = do
  describe "Flow.AST" do
    it "placeholder test" do
      True `shouldBe` True

