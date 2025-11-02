module Flow.AST.Surface.Pattern where

import "base" GHC.Generics (Generic)
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "tree-diff" Data.TreeDiff.Class (ToExpr)
import "base" Prelude hiding (Enum)

import Flow.AST.Surface.Common (SimpleVarIdentifier)
import Flow.AST.Surface.Constraint (AnyTypeIdentifier, BindersWoConstraintsF)
import Flow.AST.Surface.Literal (Literal)

data PatternF pat ty ann
  = PatSimpleF (PatternSimpleF pat ty ann)
  | PatOrF (NonEmptyVector (PatternSimpleF pat ty ann))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data PatternSimpleF pat ty ann
  = PatSimWildcardF
  | PatSimLiteralF Literal
  | PatSimVarF (PatternVariableF pat ty ann)
  | PatSimTupleF (NonEmptyVector (pat ann))
  | PatSimConstructorAppF (PatternConsturctorAppF pat ty ann)
  | PatSimOfTypeF (pat ann) (ty ann)
  | PatSimAsF (pat ann) (PatternVariableF pat ty ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data PatternVariableF pat ty ann = PatternVariableF
  { ref :: Maybe ann
  , mut :: Maybe ann
  , name :: SimpleVarIdentifier ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data PatternConsturctorAppF pat ty ann = PatternConsturctorAppF
  { name :: AnyTypeIdentifier ty ann
  , typeParams :: Maybe (BindersWoConstraintsF ty ann)
  , fields :: Maybe (PatternFieldsF pat ty ann, ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data PatternFieldsF pat ty ann
  = PatFldsUnnamedF (NonEmptyVector (PatternFieldUnnamedF pat ty ann))
  | PatFldsNamedF (NonEmptyVector (PatternFieldNamedF pat ty ann))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data PatternFieldUnnamedF pat ty ann = PatternFieldUnnamedF
  { value :: pat ann
  , optional :: Maybe ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data PatternFieldNamedF pat ty ann
  = PatFldNmdValueF (PatternFieldNamedValueF pat ty ann)
  | PatFldNmdPunningF (PatternFieldNamedPunningF pat ty ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data PatternFieldNamedValueF pat ty ann = PatternFieldNamedValueF
  { name :: SimpleVarIdentifier ann
  , value :: pat ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data PatternFieldNamedPunningF pat ty ann = PatternFieldNamedPunningF
  { ref :: Maybe ann
  , mut :: Maybe ann
  , name :: SimpleVarIdentifier ann
  , optional :: Maybe ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
