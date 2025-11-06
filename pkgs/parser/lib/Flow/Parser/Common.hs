{-# OPTIONS_GHC -Wno-orphans #-}

module Flow.Parser.Common (
  type HasAnn,
  type Parser,
  Lexer.SourceSpan (..),
  Lexer.WithPos (..),
  type Lexer.TokenWithPos,
  Lexer.TokenStream (..),
  single,
  token,
  moduleIdentifier,
  simpleTypeIdentifier,
  simpleVarIdentifier,
  regionIdentifier,
  methodIdentifier,
  pPub,
) where

import "base" Data.List.NonEmpty qualified as List (NonEmpty)
import "base" Data.List.NonEmpty qualified as List.NonEmpty
import "base" Data.String (IsString (..))
import "base" Data.Void (Void)
import "base" GHC.Records (HasField)
import "containers" Data.Set (Set)
import "containers" Data.Set qualified as Set
import "megaparsec" Text.Megaparsec (Parsec)
import "megaparsec" Text.Megaparsec qualified as Megaparsec

import Data.Char qualified as Char
import Data.Text qualified as Text
import Flow.AST.Surface.Common (
  ModuleIdentifier (..),
  RegionIdentifier (..),
  SimpleTypeIdentifier (..),
  SimpleVarIdentifier (..),
 )
import Flow.Lexer qualified as Lexer
import qualified Flow.AST.Surface.Common as Surface

type HasAnn f ann = HasField "ann" (f ann) ann

type Parser = Parsec Void Lexer.TokenStream

instance IsString (List.NonEmpty Char) where
  fromString = List.NonEmpty.fromList

single :: Lexer.Token -> Parser (Lexer.WithPos Lexer.Token)
single t = Megaparsec.satisfy ((== t) . (.value))

token ::
  Set (Megaparsec.ErrorItem (Lexer.WithPos Lexer.Token)) ->
  (Lexer.Token -> Maybe a) ->
  Parser (Lexer.WithPos a)
token expected match =
  Megaparsec.token
    ( \tw@Lexer.WithPos{value = t} -> do
        r <- match t
        pure $ tw{Lexer.value = r}
    )
    expected

moduleIdentifier :: Parser (ModuleIdentifier Lexer.SourceSpan)
moduleIdentifier = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "module identifier")
      \case
        Lexer.Identifier i -> Just i
        _ -> Nothing
  pure $ ModuleIdentifier{name = tok.value, ann = tok.span}

simpleTypeIdentifier :: Parser (SimpleTypeIdentifier Lexer.SourceSpan)
simpleTypeIdentifier = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "type identifier (should start with an uppercase letter)")
      \case
        Lexer.Identifier i
          | Char.isUpper (Text.head i) -> Just i
        _ -> Nothing
  pure $ SimpleTypeIdentifier{name = tok.value, ann = tok.span}

simpleVarIdentifier :: Parser (SimpleVarIdentifier Lexer.SourceSpan)
simpleVarIdentifier = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "variable identifier (should start with a lowercase letter)")
      \case
        Lexer.Identifier i
          | Char.isLower (Text.head i) -> Just i
        _ -> Nothing
  pure $ SimpleVarIdentifier{name = tok.value, ann = tok.span}

regionIdentifier :: Parser (RegionIdentifier Lexer.SourceSpan)
regionIdentifier = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "region identifier (should start with a single quote)")
      \case
        Lexer.Region i -> Just i
        _ -> Nothing
  pure $ RegionIdentifier{name = tok.value, ann = tok.span}

methodIdentifier :: Parser (SimpleVarIdentifier Lexer.SourceSpan)
methodIdentifier = do
  dotTok <- single (Lexer.Punctuation Lexer.Dot)
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "method identifier (should start with lowercase letter)")
      \case
        Lexer.Identifier i
          | Char.isLower (Text.head i) -> Just i
        _ -> Nothing
  pure
    SimpleVarIdentifier
      { name = tok.value
      , ann = Lexer.SourceSpan dotTok.span.start tok.span.end
      }

pPub :: Parser (Surface.Pub Lexer.SourceSpan, Lexer.SourceSpan)
pPub = do
  pubTok <- single (Lexer.Keyword Lexer.Pub)
  packageWithEnd <- Megaparsec.optional do
    _ <- single (Lexer.Punctuation Lexer.LeftParen)
    package' <- token
      (Set.singleton $ Megaparsec.Label "package")
      \case
        Lexer.Identifier i
          | i == "package " -> Just ()
        _ -> Nothing
    tokE <- single (Lexer.Punctuation Lexer.RightParen)
    let ann = Lexer.SourceSpan{start = package'.span.start, end = package'.span.end}
    pure (Surface.PubPackage ann, tokE.span.end)
  case packageWithEnd of
    Just (package', end) -> pure (package', Lexer.SourceSpan{start = pubTok.span.start, end})
    Nothing -> pure (Surface.PubPub, pubTok.span)
