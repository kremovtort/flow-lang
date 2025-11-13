{-# LANGUAGE MultilineStrings #-}

module Flow.Core.ModuleSpec (spec) where

import Prelude

import "base" Data.Foldable (toList)
import "base" GHC.Stack (HasCallStack)
import "containers" Data.Sequence (Seq)
import "containers" Data.Sequence qualified as Seq
import "effectful" Effectful (runPureEff)
import "effectful" Effectful.Error.Static (runErrorNoCallStack)
import "hspec" Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "text" Data.Text (Text)
import "vector" Data.Vector qualified as Vector

import Flow.AST.Ann (SourceSpan)
import Flow.AST.Surface (ModDefinitionBody)
import Flow.Core.Module (
  BasicAnn (..),
  DuplicateModuleDeclarationError (..),
  ModuleId (..),
  collectModuleDeclarations,
 )
import Flow.Core.Package (PackageId (..))
import Flow.Lexer qualified as Lexer
import Flow.Parser (pModDefinitionBody)
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Maybe (fromJust)
import qualified Data.Vector.NonEmpty as NonEmptyVector

spec :: Spec
spec = describe "collectModuleDeclarations" do
  it "collects declarations" do
    body <-
      parseModuleBody
        """
        pub mod foo;
        pub(package) mod bar;
        mod baz;
        """
    let result = runCollect rootModuleId body
    result
      `shouldBe` Right
        ( Seq.fromList
            [ mkNEVector ["foo"]
            , mkNEVector ["bar"]
            , mkNEVector ["baz"]
            ]
        )

  it "fails on duplicate module declarations" do
    body <-
      parseModuleBody
        """
        mod foo {}
        mod foo;
        """
    case runCollect rootModuleId body of
      Left (DuplicateModuleDeclarationError moduleId _) ->
        moduleId `shouldBe` mkModuleId ["foo"]
      other ->
        expectationFailure $
          "expected duplicate declaration error, but got: " <> show other

  it "collects declarations from nested module definitions" do
    body <-
      parseModuleBody
        """
        mod outer {
          pub mod inner;
          mod nested {
            pub mod deep;
          }
          mod leaf;
        }
        pub mod top;
        """
    let result = runCollect rootModuleId body
    result
      `shouldBe` Right
        ( Seq.fromList
            [ mkNEVector ["outer", "inner"]
            , mkNEVector ["outer", "nested", "deep"]
            , mkNEVector ["outer", "leaf"]
            , mkNEVector ["top"]
            ]
        )

testPackageId :: PackageId
testPackageId = PackageId "test"

rootModuleId :: ModuleId
rootModuleId = mkModuleId []

mkModuleId :: [Text] -> ModuleId
mkModuleId parts =
  ModuleId
    { package = testPackageId
    , name = Vector.fromList parts
    }

mkNEVector :: HasCallStack => [Text] -> NonEmptyVector Text
mkNEVector parts = fromJust $ NonEmptyVector.fromList parts

runCollect ::
  (HasCallStack) =>
  ModuleId ->
  ModDefinitionBody BasicAnn ->
  Either DuplicateModuleDeclarationError (Seq (NonEmptyVector Text))
runCollect moduleId body =
  runPureEff $ runErrorNoCallStack $ fmap fst <$> collectModuleDeclarations moduleId body

parseModuleBody :: (HasCallStack) => Text -> IO (ModDefinitionBody BasicAnn)
parseModuleBody source = do
  tokens <- case Megaparsec.runParser Lexer.tokensWithPos testPath source of
    Left err -> parseFailure "lexer" err
    Right toks -> pure toks
  let tokenStream =
        Lexer.TokenStream
          { tokens = toList tokens
          , inputPos = Megaparsec.initialPos testPath
          , input = source
          }
  case Megaparsec.runParser pModDefinitionBody testPath tokenStream of
    Left err -> parseFailure "parser" err
    Right body -> pure $ annotate body
 where
  annotate :: ModDefinitionBody SourceSpan -> ModDefinitionBody BasicAnn
  annotate = fmap \span' -> BasicAnn{path = testPath, span = span'}

  parseFailure stage err = do
    expectationFailure $
      "Failed during " <> stage <> ":\n" <> Megaparsec.errorBundlePretty err
    fail "parseModuleBody: unreachable"

testPath :: FilePath
testPath = "<collect-module-spec>"
