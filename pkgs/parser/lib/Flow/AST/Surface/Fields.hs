module Flow.AST.Surface.Fields where

import "base" Prelude hiding (Enum)
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)

import Flow.AST.Common (SimpleVarIdentifier)

data Fields v
  = FieldsTuple (NonEmptyVector v)
  | FieldsNamed (NonEmptyVector (SimpleVarIdentifier, v))
  deriving (Eq, Ord, Show)
