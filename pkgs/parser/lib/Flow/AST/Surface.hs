module Flow.AST.Surface where

import Flow.AST.Surface.Expr (ExpressionF)
import Flow.AST.Surface.Module (ModF, ModDefinitionBodyF)
import Flow.AST.Surface.Pattern (PatternF)
import Flow.AST.Surface.Syntax (LHSExpressionF)
import Flow.AST.Surface.Type (TypeF)

newtype ModDefinitionBody ann = ModDefinitionBody (ModDefinitionBodyF Mod LHSExpression Pattern Type Expression ann)

newtype Mod ann = Mod (ModF Mod LHSExpression Pattern Type Expression ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype LHSExpression ann = LHSExpression (LHSExpressionF LHSExpression Expression ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Pattern ann = Pattern (PatternF Pattern ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Type ann = Type (TypeF Type ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Expression ann = Expression (ExpressionF LHSExpression Pattern Type Expression ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
