module Flow.Parser.Helpers where

import "base" Data.Foldable (toList)
import "base" Data.Functor (void)
import "hspec" Test.Hspec (expectationFailure)
import "megaparsec" Text.Megaparsec (parse)
import "text" Data.Text (Text)

import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (Parser)
import Text.Megaparsec qualified as Megaparsec

lexTokens :: Text -> IO Lexer.TokenStream
lexTokens s = case parse Lexer.tokensWithPos "<lex>" s of
  Left e -> do
    fail
      ("Lexer failed to tokenize:\n" <> Megaparsec.errorBundlePretty e)
  Right v -> pure Lexer.TokenStream
    { tokens = toList v
    , inputPos = Megaparsec.initialPos "<lex>"
    , input = s
    }

testParser ::
  (Functor f, Eq (f ()), Show (f ())) =>
  Text ->
  Parser (f Lexer.SourceRegion) ->
  Maybe (f ()) ->
  IO ()
testParser txt parser expected = do
  toks <- lexTokens txt
  case parse (void <$> (parser <* Megaparsec.eof)) "<parser>" toks of
    Left e ->
      case expected of
        Nothing -> pure ()
        Just _ -> expectationFailure ("Parser failed to parse:\n" <> Megaparsec.errorBundlePretty e)
    Right v ->
      case expected of
        Just expected' ->
          if v == expected'
            then pure ()
            else expectationFailure ("AST mismatch. Got:\n" <> show v <> "\nExpected:\n" <> show expected')
        Nothing -> expectationFailure ("Parser should fail but parsed:\n" <> show v)
