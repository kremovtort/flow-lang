module Flow.Parser.Constraint where

import "base" Control.Monad (unless, when)
import "base" Data.Functor (void)
import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector
import "vector" Data.Vector qualified as Vector

import Data.Maybe (fromMaybe)
import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Constraint (ScopeBinderWoConstraintsF (..))
import Flow.AST.Surface.Constraint qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (
  HasAnn,
  Parser,
  moduleIdentifier,
  scopeIdentifier,
  simpleTypeIdentifier,
  simpleVarIdentifier,
  single,
 )

pBindersApp ::
  (HasAnn ty Lexer.SourceRegion) =>
  Parser (ty Lexer.SourceRegion) ->
  Parser (Surface.BindersAppF ty Lexer.SourceRegion)
pBindersApp pTy = do
  tokS <- single (Lexer.Punctuation Lexer.LessThan)
  scopes <-
    Vector.fromList . fmap ScopeBinderWoConstraintsF
      <$> Megaparsec.sepBy scopeIdentifier (single (Lexer.Punctuation Lexer.Comma))
  types <-
    Vector.fromList . fmap Surface.BinderAppF
      <$> if null scopes
        then Megaparsec.sepEndBy1 pTy (single (Lexer.Punctuation Lexer.Comma))
        else do
          _ <- single (Lexer.Punctuation Lexer.Comma)
          Megaparsec.sepEndBy pTy (single (Lexer.Punctuation Lexer.Comma))
  tokE <- single (Lexer.Punctuation Lexer.GreaterThan)
  pure
    Surface.BindersF
      { scopes
      , types
      , ann = Lexer.SourceRegion tokS.region.start tokE.region.end
      }

anyTypeIdentifier ::
  forall ty.
  (HasAnn ty Lexer.SourceRegion) =>
  Parser (ty Lexer.SourceRegion) ->
  Parser (Surface.AnyTypeIdentifier ty Lexer.SourceRegion)
anyTypeIdentifier pTy = do
  qualifier <- Megaparsec.many (Megaparsec.try (moduleIdentifier <* moduleSeparator))
  typeQualifier <- Megaparsec.optional $ Megaparsec.try do
    typeName <- simpleTypeIdentifier
    typeParams <- pBindersApp pTy
    _ <- single (Lexer.Punctuation Lexer.ColonColon)
    pure
      Surface.TypeQualifierF
        { typeName
        , typeParams
        }
  identifier <- simpleTypeIdentifier
  pure $
    Surface.AnyTypeIdentifier
      { qualifier = NonEmptyVector.fromList qualifier
      , typeQualifier
      , identifier
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

anyVarIdentifier ::
  (HasAnn ty Lexer.SourceRegion) =>
  Parser (ty Lexer.SourceRegion) ->
  Parser (Surface.AnyVarIdentifier ty Lexer.SourceRegion)
anyVarIdentifier pTy = do
  qualifier <- Megaparsec.many (Megaparsec.try (moduleIdentifier <* moduleSeparator))
  typeQualifier <- Megaparsec.optional do
    typeName <- simpleTypeIdentifier
    typeParams <- pBindersApp pTy
    pure
      Surface.TypeQualifierF
        { typeName
        , typeParams
        }
  identifier <- simpleVarIdentifier
  pure $
    Surface.AnyVarIdentifier
      { qualifier = NonEmptyVector.fromList qualifier
      , typeQualifier = typeQualifier
      , identifier = identifier
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

pBindersWoConstraints ::
  forall ty.
  (HasAnn ty Lexer.SourceRegion) =>
  Parser (ty Lexer.SourceRegion) ->
  Parser (Surface.BindersWoConstraintsF ty Lexer.SourceRegion)
pBindersWoConstraints pTy = Megaparsec.label "binders with constraints" do
  tokS <- single (Lexer.Punctuation Lexer.LessThan)
  scopeBinders <- Megaparsec.sepBy scopeIdentifier (single (Lexer.Punctuation Lexer.Comma))
  typeBinders <- Megaparsec.optional do
    unless (null scopeBinders) do
      void $ single (Lexer.Punctuation Lexer.Comma)
    Megaparsec.sepBy1 pBinderWoConstraints (single (Lexer.Punctuation Lexer.Comma))
  when (null scopeBinders && null typeBinders) do
    fail "Expected at least one scope or type binder"
  _ <- Megaparsec.optional $ single (Lexer.Punctuation Lexer.Comma)
  tokE <- single (Lexer.Punctuation Lexer.GreaterThan)
  pure
    ( Surface.BindersF
        { scopes = Vector.fromList (fmap Surface.ScopeBinderWoConstraintsF scopeBinders)
        , types = Vector.fromList (fromMaybe [] typeBinders)
        , ann = Lexer.SourceRegion{start = tokS.region.start, end = tokE.region.end}
        }
    )
 where
  pBinderWoConstraints = do
    name <- simpleTypeIdentifier
    typeType <- Megaparsec.optional do
      _ <- single (Lexer.Punctuation Lexer.Colon)
      pTy
    pure $
      Surface.BinderWoConstraintF
        { name = name
        , typeType = typeType
        , ann =
            Lexer.SourceRegion
              { start = name.ann.start
              , end = case typeType of
                  Just ty -> ty.ann.end
                  Nothing -> name.ann.end
              }
        }
