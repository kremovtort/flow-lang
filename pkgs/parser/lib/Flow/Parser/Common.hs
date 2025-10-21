{-# OPTIONS_GHC -Wno-orphans #-}

module Flow.Parser.Common (
  type HasAnn,
  type Parser,
  Lexer.SourceRegion(..),
  Lexer.WithPos(..),
  type Lexer.TokenWithPos,
  Lexer.TokenStream(..),
  single,
  foldPos,
  dummySourceRegion,
  token,
  moduleIdentifier,
  simpleTypeIdentifier,
  simpleVarIdentifier,
  anyTypeIdentifier,
  anyVarIdentifier,
  scopeIdentifier,
) where

import "base" Data.List.NonEmpty qualified as List (NonEmpty)
import "base" Data.List.NonEmpty qualified as List.NonEmpty
import "base" Data.String (IsString (..))
import "base" Data.Void (Void)
import "containers" Data.Set (Set)
import "containers" Data.Set qualified as Set
import "megaparsec" Text.Megaparsec (Parsec)
import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "base" GHC.Records (HasField)

import Data.Char qualified as Char
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Flow.AST.Surface.Common (
  AnyTypeIdentifier (..),
  AnyVarIdentifier (..),
  ModuleIdentifier (..),
  ScopeIdentifier (..),
  SimpleTypeIdentifier (..),
  SimpleVarIdentifier (..),
 )
import Flow.Lexer qualified as Lexer

type HasAnn f ann = HasField "ann" (f ann) ann

type Parser = Parsec Void Lexer.TokenStream

instance IsString (List.NonEmpty Char) where
  fromString = List.NonEmpty.fromList

single :: Lexer.Token -> Parser (Lexer.WithPos Lexer.Token)
single t = Megaparsec.satisfy ((== t) . (.value))

foldPos :: (Foldable f) => f Lexer.SourceRegion -> Lexer.SourceRegion
foldPos = foldr1 (\r acc -> Lexer.SourceRegion{start = r.start, end = acc.end})

dummySourceRegion :: Lexer.SourceRegion
dummySourceRegion =
  Lexer.SourceRegion
    { start = Megaparsec.initialPos "dummy"
    , end = Megaparsec.initialPos "dummy"
    }

token ::
  Set (Megaparsec.ErrorItem Lexer.Token) ->
  (Lexer.Token -> Maybe a) ->
  Parser (Lexer.WithPos a)
token expected match =
  Megaparsec.token
    ( \tw@Lexer.WithPos{value = t} -> do
        r <- match t
        pure $ tw{Lexer.value = r}
    )
    ( Set.map
        (fmap (\t -> Lexer.WithPos{value = t, region = dummySourceRegion}))
        expected
    )

moduleIdentifier :: Parser (ModuleIdentifier Lexer.SourceRegion)
moduleIdentifier = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "module identifier")
      \case
        Lexer.Identifier i -> Just i
        _ -> Nothing
  pure $ ModuleIdentifier{name = tok.value, ann = tok.region}

simpleTypeIdentifier :: Parser (SimpleTypeIdentifier Lexer.SourceRegion)
simpleTypeIdentifier = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "type identifier (should start with an uppercase letter)")
      \case
        Lexer.Identifier i
          | Char.isUpper (Text.head i) -> Just i
        _ -> Nothing
  pure $ SimpleTypeIdentifier{name = tok.value, ann = tok.region}

simpleVarIdentifier :: Parser (SimpleVarIdentifier Lexer.SourceRegion)
simpleVarIdentifier = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "variable identifier (should start with a lowercase letter)")
      \case
        Lexer.Identifier i
          | Char.isLower (Text.head i) -> Just i
        _ -> Nothing
  pure $ SimpleVarIdentifier{name = tok.value, ann = tok.region}

anyTypeIdentifier :: Parser (AnyTypeIdentifier Lexer.SourceRegion)
anyTypeIdentifier = do
  qualifier <- Megaparsec.many (Megaparsec.try (moduleIdentifier <* moduleSeparator))
  identifier <- simpleTypeIdentifier
  pure $
    AnyTypeIdentifier
      { qualifier = Vector.fromList qualifier
      , qualifierAnn = case qualifier of
          [] -> Nothing
          q : _ -> Just $ Lexer.SourceRegion q.ann.start (last qualifier).ann.end
      , identifier = identifier
      , identifierAnn = identifier.ann
      , ann =
          Lexer.SourceRegion
            { start = case qualifier of
                [] -> identifier.ann.start
                q : _ -> q.ann.start
            , end = identifier.ann.end
            }
      }
 where
  moduleSeparator = single (Lexer.Punctuation Lexer.ColonColon)

anyVarIdentifier :: Parser (AnyVarIdentifier Lexer.SourceRegion)
anyVarIdentifier = do
  qualifier <- Megaparsec.many (Megaparsec.try (moduleIdentifier <* moduleSeparator))
  identifier <- simpleVarIdentifier
  pure $
    AnyVarIdentifier
      { qualifier = Vector.fromList qualifier
      , qualifierAnn = case qualifier of
          [] -> Nothing
          q : _ -> Just $ Lexer.SourceRegion q.ann.start (last qualifier).ann.end
      , identifier = identifier
      , identifierAnn = identifier.ann
      , ann =
          Lexer.SourceRegion
            { start = case qualifier of
                [] -> identifier.ann.start
                q : _ -> q.ann.start
            , end = identifier.ann.end
            }
      }
 where
  moduleSeparator = single (Lexer.Punctuation Lexer.ColonColon)

scopeIdentifier :: Parser (ScopeIdentifier Lexer.SourceRegion)
scopeIdentifier = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "scope identifier (should start with a single quote)")
      \case
        Lexer.RefScope i -> Just i
        _ -> Nothing
  pure $ ScopeIdentifier{name = tok.value, ann = tok.region}
