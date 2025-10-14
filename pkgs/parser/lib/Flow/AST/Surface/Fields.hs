module Flow.AST.Surface.Fields where

import "base" Prelude hiding (Enum)
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)

import Flow.AST.Surface.Common (SimpleVarIdentifier)

data Fields inner ann
  = FieldsTuple (NonEmptyVector (inner ann)) ann
  | FieldsNamed (NonEmptyVector (SimpleVarIdentifier ann, inner ann)) ann
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
