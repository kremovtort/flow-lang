module Flow.Parser.Constraint where

import "base" Control.Monad (unless, when)
import "base" Data.Functor (void)
import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "vector" Data.Vector qualified as Vector

import Data.Maybe (fromMaybe)
import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Constraint qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (Parser, scopeIdentifier, simpleTypeIdentifier, single, HasAnn)

pBindersWoConstraints ::
  forall ty.
  HasAnn ty Lexer.SourceRegion =>
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
        , ann = Lexer.SourceRegion{start = tokS.payload.start, end = tokE.payload.end}
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
