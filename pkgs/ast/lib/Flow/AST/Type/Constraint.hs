module Flow.AST.Type.Constraint where

import "vector" Data.Vector (Vector)

import Flow.AST.Type.Common (Name)
import Flow.AST.Type.Binder (TyBinder)

-- | Saturated constraint predicate, Core-level, parameterized by the type node.
data ConstraintF ty
  = ConstraintApp Name (Vector ty) -- C<T1,...,Tn>
  | ConstraintForall (Vector TyBinder) (ConstraintF ty) -- <A: ...> D<...>
  deriving (Eq, Ord, Show)


