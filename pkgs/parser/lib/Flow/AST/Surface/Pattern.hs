module Flow.AST.Surface.Pattern where

import "base" Prelude hiding (Enum)
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
-- imports only for instances; keep empty to avoid unused warnings
import "scientific" Data.Scientific ()
import "text" Data.Text ()
import "bytestring" Data.ByteString ()
import "base" Data.Word ()
import "base" Data.Char ()

import Flow.AST.Surface.Common (SimpleVarIdentifier)
import Flow.AST.Surface.Literal (Literal)
import Flow.AST.Surface.Syntax (ConstructorApp)

data PatternF pat ty ann
  = PatternSimpleF (PatternSimpleF pat ty ann) ann
  | PatternOrF (NonEmptyVector (PatternSimpleF pat ty ann)) ann
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data PatternSimpleF pat ty ann
  = PatternSimpleWildcardF ann
  | PatternSimpleLiteralF (Literal ann) ann
  | PatternSimpleVarF (SimpleVarIdentifier ann) ann
  | PatternSimpleTupleF (NonEmptyVector (pat ann)) ann
  | PatternSimpleConsF (ConstructorApp pat ty ann) ann
  | PatternSimpleOfTypeF (pat ann) (ty ann) ann
  | PatternSimpleAsF (pat ann) (SimpleVarIdentifier ann) ann
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
