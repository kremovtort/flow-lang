module Flow.Parser.Helpers where

import "base" Data.Foldable (toList)
import "hspec" Test.Hspec (expectationFailure)
import "megaparsec" Text.Megaparsec (parse)
import "text" Data.Text (Text)

import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (Parser)

lexTokens :: Text -> IO [Lexer.TokenWithSourceRegion]
lexTokens s = case parse Lexer.tokensWithSourceRegion "<lex>" s of
  Left e -> expectationFailure ("Lexer failed to tokenize: " <> show e) >> pure []
  Right v -> pure (toList v)

testParser ::
  (Functor f, Eq (f ()), Show (f ())) =>
  Text ->
  Parser (f Lexer.SourceRegion) ->
  Maybe (f ()) ->
  IO ()
testParser txt parser expected = do
  toks <- lexTokens txt
  case parse (fmap (const ()) <$> parser) "<parser>" toks of
    Left e ->
      case expected of
        Nothing -> pure ()
        Just _ -> expectationFailure ("Parser failed to parse: " <> show e)
    Right v ->
      case expected of
        Just expected' ->
          if v == expected'
            then pure ()
            else expectationFailure ("AST mismatch. Got: " <> show v <> ", expected: " <> show expected')
        Nothing -> expectationFailure ("Parser should fail but parsed: " <> show v)
