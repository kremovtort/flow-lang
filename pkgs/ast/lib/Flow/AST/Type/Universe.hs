module Flow.AST.Type.Universe where

data Sort
  = Type
  | Constraint
  | Effect
  | RefScope
  deriving (Eq, Ord, Show, Bounded, Enum)
