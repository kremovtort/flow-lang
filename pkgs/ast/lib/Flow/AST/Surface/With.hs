module Flow.AST.Surface.With where

import "base" GHC.Generics (Generic)
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "tree-diff" Data.TreeDiff.Class (ToExpr)
import "base" Prelude hiding (Enum)

import Flow.AST.Surface.Common (SimpleVarIdentifier)
import Flow.AST.Surface.Syntax (CodeBlockF)

data WithAppF ty expr ann = WithAppF
  { fields :: WithAppFieldsF ty expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data WithAppFieldsF ty expr ann
  = WAppUnnamedF (NonEmptyVector (WithRhsF ty expr ann))
  | WAppNamedF (NonEmptyVector (WithAppNamedClauseF ty expr ann))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data WithAppNamedClauseF ty expr ann = WithAppNamedClauseF
  { lhs :: NonEmptyVector (WithLhsF ty ann)
  , rhs :: WithRhsF ty expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data WithBlockF stmt ty expr ann = WithBlockF
  { withStatements :: NonEmptyVector (WithStatementF ty expr ann)
  , block :: CodeBlockF stmt expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data WithStatementF ty expr ann = WithStatementF
  { let_ :: Maybe ann
  , lhs :: NonEmptyVector (WithLhsF ty ann)
  , rhs :: WithRhsF ty expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data WithLhsF ty ann
  = WLhsLabelledF (SimpleVarIdentifier ann) (Maybe (ty ann))
  | WLhsUnlabelledF (ty ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data WithRhsF ty expr ann
  = WRhsExprF (WithRhsExprF ty expr ann)
  | WRhsTypeF (ty ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data WithRhsExprF ty expr ann = WithRhsExprF
  { expr :: expr ann
  , in_ :: Maybe (NonEmptyVector (InStatementF ty expr ann))
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data InStatementF ty expr ann = InStatementF
  { lhs :: NonEmptyVector (ty ann)
  , rhs :: WithRhsF ty expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
