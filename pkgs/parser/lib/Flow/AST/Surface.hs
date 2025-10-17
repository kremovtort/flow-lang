module Flow.AST.Surface where

import Flow.AST.Surface.Expr (ExpressionF)
import Flow.AST.Surface.Module (ModDefinitionBodyF, ModF)
import Flow.AST.Surface.Pattern (PatternF, PatternSimpleF)
import Flow.AST.Surface.Syntax (LHSExpressionF)
import Flow.AST.Surface.Type (TypeF)

data ModDefinitionBody ann = ModDefinitionBody
  { modDefinitionBody :: ModDefinitionBodyF Mod LHSExpression PatternSimple Pattern Type Expression ann
  , ann :: ann
  }

data Mod ann = Mod
  { mod :: ModF Mod LHSExpression PatternSimple Pattern Type Expression ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data LHSExpression ann = LHSExpression
  { lhsExpression :: LHSExpressionF LHSExpression Expression ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Pattern ann = Pattern
  { pattern :: PatternF Pattern Type ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data PatternSimple ann = PatternSimple
  { patternSimple :: PatternSimpleF PatternSimple Type ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Type ann = Type
  { ty :: TypeF Type ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Expression ann = Expression
  { expr :: ExpressionF LHSExpression PatternSimple Pattern Type Expression ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
