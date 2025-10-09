module Flow.AST.Core.Type.Alias where

import "vector" Data.Vector (Vector)

import Flow.AST.Core.Type.Common (Name)
import Flow.AST.Core.Type.Binder (TyBinder)
import Flow.AST.Core.Type.Effect (EffectF)
import Flow.AST.Core.Type.Constraint (ConstraintF)

-- | Alias body can inhabit any universe: Type, Constraint, Effect, RefScope.
-- We model only those needed for Core typing in signatures: Type, Constraint, Effect.
data AliasBody ty
  = AliasType ty
  | AliasConstraint (ConstraintF ty)
  | AliasEffect (EffectF ty)
  deriving (Eq, Ord, Show)

-- | Local alias declaration. Recursion is disallowed by elaboration checker.
data AliasDecl ty = AliasDecl
  { aliasName :: Name
  , aliasBinders :: Vector TyBinder      -- parameters, with explicit universes
  , aliasBody :: AliasBody ty
  }
  deriving (Eq, Ord, Show)


