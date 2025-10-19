module Flow.AST.Surface.Pattern where

import "base" Prelude hiding (Enum)
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
-- imports only for instances; keep empty to avoid unused warnings
import "scientific" Data.Scientific ()
import "text" Data.Text ()
import "bytestring" Data.ByteString ()
import "base" Data.Word ()
import "base" Data.Char ()

import Flow.AST.Surface.Common (SimpleVarIdentifier, SimpleTypeIdentifier)
import Flow.AST.Surface.Literal (Literal)
import Flow.AST.Surface.Syntax (ConstructorAppF)

data PatternF pat ty ann
  = PatternSimpleF (PatternSimpleF pat ty ann) ann
  | PatternOrF (NonEmptyVector (PatternSimpleF pat ty ann)) ann
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data PatternSimpleF pat ty ann
  = PatternSimpleWildcardF
  | PatternSimpleLiteralF Literal
  | PatternSimpleVarF (SimpleVarIdentifier ann)
  | PatternSimpleTupleF (NonEmptyVector (pat ann))
  | PatternSimpleConsF (ConstructorAppF pat SimpleTypeIdentifier ann)
  | PatternSimpleOfTypeF (pat ann) (ty ann)
  | PatternSimpleAsF (pat ann) (SimpleVarIdentifier ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
