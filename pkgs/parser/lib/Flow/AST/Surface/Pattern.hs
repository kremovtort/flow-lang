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
import Flow.AST.Surface.Constraint (BindersWoConstraintsF)

data PatternF pat ty ann
  = PatSimpleF (PatternSimpleF pat ty ann)
  | PatOrF (NonEmptyVector (PatternSimpleF pat ty ann))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data PatternSimpleF pat ty ann
  = PatSimWildcardF
  | PatSimLiteralF Literal
  | PatSimVarF (PatternVariableF pat ty ann)
  | PatSimTupleF (NonEmptyVector (pat ann))
  | PatSimConstructorAppF (PatternConsturctorAppF pat ty ann)
  | PatSimOfTypeF (pat ann) (ty ann)
  | PatSimAsF (pat ann) (PatternVariableF pat ty ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data PatternVariableF pat ty ann = PatternVariableF
  { mut :: Maybe ann
  , name :: SimpleVarIdentifier ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data PatternConsturctorAppF pat ty ann = PatternConsturctorAppF
  { name :: AnyTypeIdentifier ann
  , typeParams :: Maybe (BindersWoConstraintsF ty ann)
  , fields :: Maybe (PatternFieldsF pat ty ann, ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data PatternFieldsF pat ty ann
  = PatFldsUnnamedF (NonEmptyVector (PatternFieldUnnamedF pat ty ann))
  | PatFldsNamedF (NonEmptyVector (PatternFieldNamedF pat ty ann))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data PatternFieldUnnamedF pat ty ann = PatternFieldUnnamedF
  { value :: pat ann
  , optional :: Maybe ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data PatternFieldNamedF pat ty ann
  = PatFldNmdValueF (PatternFieldNamedValueF pat ty ann)
  | PatFldNmdPunningF (PatternFieldNamedPunningF pat ty ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data PatternFieldNamedValueF pat ty ann = PatternFieldNamedValueF
  { name :: SimpleVarIdentifier ann
  , value :: pat ann
  , optional :: Maybe ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data PatternFieldNamedPunningF pat ty ann = PatternFieldNamedPunningF
  { mut :: Maybe ann
  , name :: SimpleVarIdentifier ann
  , optional :: Maybe ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
