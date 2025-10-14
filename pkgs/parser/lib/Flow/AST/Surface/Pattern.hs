module Flow.AST.Surface.Pattern where

import "base" Prelude hiding (Enum)
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
-- imports only for instances; keep empty to avoid unused warnings
import "scientific" Data.Scientific ()
import "text" Data.Text ()
import "bytestring" Data.ByteString ()
import "base" Data.Word ()
import "base" Data.Char ()

import Flow.AST.Surface.Common (SimpleVarIdentifier, AnyTypeIdentifier)
import Flow.AST.Surface.Literal (Literal)
import Flow.AST.Surface.Fields (Fields)

data PatternF pat ann
  = PatternWildcardF ann
  | PatternLiteralF (Literal ann) ann
  | PatternVarF (SimpleVarIdentifier ann) ann
  | PatternTupleF (NonEmptyVector (pat ann)) ann
  | PatternConsF (ConstructorApp pat ann) ann
  | PatternOrF (NonEmptyVector (pat ann)) ann
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data ConstructorApp a ann = ConstructorApp
  { name :: AnyTypeIdentifier ann
  , fields :: Fields a ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
