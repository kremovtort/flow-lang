module Flow.Parser.Helpers where

import "base" Data.Foldable (toList)
import "base" Data.Functor (void)
import "base" GHC.Stack (HasCallStack, withFrozenCallStack)
import "hspec" Test.Hspec (expectationFailure)
import "megaparsec" Text.Megaparsec (parse)
import "text" Data.Text (Text)
import "text" Data.Text qualified as Text
import "tree-diff" Data.TreeDiff.Class (ToExpr (..), ediff)

import Data.TreeDiff.Pretty (ansiWlBgEditExprCompact, ansiWlExpr)
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (Parser)
import Prettyprinter (defaultLayoutOptions, layoutSmart)
import Prettyprinter.Render.Terminal (renderStrict)
import Text.Megaparsec qualified as Megaparsec

pShow :: (ToExpr a) => a -> Text
pShow = renderStrict . layoutSmart defaultLayoutOptions . ansiWlExpr . toExpr

pShowDiff :: (Eq a, ToExpr a) => a -> a -> Text
pShowDiff a b =
  renderStrict $
    layoutSmart defaultLayoutOptions $
      ansiWlBgEditExprCompact $
        ediff a b

shouldBe :: (Eq a, ToExpr a, HasCallStack) => a -> a -> IO ()
shouldBe a b
  | a == b = pure ()
  | otherwise = do
      expectationFailure $ Text.unpack $ "Equation failed:\n" <> pShowDiff a b

lexTokens :: Text -> IO Lexer.TokenStream
lexTokens s = case parse Lexer.tokensWithPos "<lex>" s of
  Left e -> do
    fail
      ("Lexer failed to tokenize:\n" <> Megaparsec.errorBundlePretty e)
  Right v ->
    pure
      Lexer.TokenStream
        { tokens = toList v
        , inputPos = Megaparsec.initialPos "<lex>"
        , input = s
        }

testParser ::
  ( HasCallStack
  , Functor f
  , Eq (f ())
  , Show (f ())
  , ToExpr (f ())
  ) =>
  Text ->
  Parser (f Lexer.SourceRegion) ->
  Maybe (f ()) ->
  IO ()
testParser txt parser expected = withFrozenCallStack do
  toks <- lexTokens txt
  case parse (void <$> (parser <* Megaparsec.eof)) "<parser>" toks of
    Left e ->
      case expected of
        Nothing -> pure ()
        Just _ -> expectationFailure ("Parser failed to parse:\n" <> Megaparsec.errorBundlePretty e)
    Right v ->
      case expected of
        Just expected' ->
          expected' `shouldBe` v
        Nothing -> do
          expectationFailure $ Text.unpack $ "Parser should fail but parsed:\n" <> pShow v
