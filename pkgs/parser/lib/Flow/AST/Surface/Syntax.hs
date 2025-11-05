module Flow.AST.Surface.Syntax where

import "base" GHC.Generics (Generic)
import "tree-diff" Data.TreeDiff.Class (ToExpr)
import "vector" Data.Vector (Vector)
import "base" Prelude hiding (Enum)

import Data.Vector.NonEmpty (NonEmptyVector)
import Flow.AST.Surface.Common (SimpleVarIdentifier, ScopeIdentifier)
import Flow.AST.Surface.Use (UseClause)

data UnitF a = UnitF
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

-- Statements and related nodes
data StatementF stmt lhsExpr simPat pat ty expr ann
  = SLetF (LetDefinitionF simPat ty expr ann)
  | SAssignF (AssignStatementF lhsExpr expr ann)
  | SReturnF (expr ann) ann
  | SContinueF (Maybe (SimpleVarIdentifier ann)) ann
  | SBreakF (Maybe (SimpleVarIdentifier ann)) ann
  | SMatchF (MatchExpressionF pat expr ann)
  | SIfF (IfExpressionF stmt pat expr ann)
  | SLoopF (LoopExpressionF stmt expr ann)
  | SWhileF (WhileStatementF stmt pat expr ann)
  | SForF (ForStatementF stmt simPat expr ann)
  | SExpressionF (expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data LetDefinitionF simPat ty expr ann = LetDefinitionF
  { lhs :: simPat ann
  , lhsType :: Maybe (ty ann)
  , rhs :: expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data AssignStatementF lhsExpr expr ann = AssignStatementF
  { lhs :: lhsExpr ann
  , rhs :: expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data LHSExpressionF lhsExpr expr ann
  = LHSEWildcard
  | LHSEVar (SimpleVarIdentifier ann)
  | LHSEIndex (lhsExpr ann) (expr ann)
  | LHSEDotAccess (lhsExpr ann) (SimpleVarIdentifier ann)
  | LHSEUnOp (LHSUnOpExpression expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

newtype LHSUnOpExpression expr ann
  = LHSUnOpExpressionDeref (expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (ToExpr)

data CodeBlockF stmt expr ann = CodeBlockF
  { region :: Maybe (ScopeIdentifier ann)
  , uses :: Vector (UseClause ann)
  , statements :: Vector (stmt ann)
  , result :: Maybe (expr ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

-- Match and control flow

data MatchExpressionF pat expr ann = MatchExpressionF
  { value :: expr ann
  , arms :: NonEmptyVector (MatchArmF pat expr ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data MatchArmF pat expr ann = MatchArmF
  { pattern :: pat ann
  , guard :: Maybe (expr ann)
  , expression :: expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data IfExpressionF stmt pat expr ann = IfExpressionF
  { branches :: NonEmptyVector (IfBranchF stmt pat expr ann)
  , else_ :: Maybe (CodeBlockF stmt expr ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data IfBranchF stmt pat expr ann = IfBranchF
  { condition :: ConditionF pat expr ann
  , result :: CodeBlockF stmt expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ConditionF pat expr ann
  = CondBoolF (expr ann)
  | CondLetF (LetConditionF pat expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data LetConditionF pat expr ann = LetConditionF
  { pattern :: pat ann
  , patternExpr :: expr ann
  , bool :: Maybe (expr ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data LoopExpressionF stmt expr ann = LoopExpressionF
  { label :: Maybe (SimpleVarIdentifier ann)
  , body :: CodeBlockF stmt expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data WhileStatementF stmt pat expr ann = WhileStatementF
  { label :: Maybe (SimpleVarIdentifier ann)
  , condition :: ConditionF pat expr ann
  , body :: CodeBlockF stmt expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ForStatementF stmt simPat expr ann = ForStatementF
  { label :: Maybe (SimpleVarIdentifier ann, ann)
  , pattern :: simPat ann
  , iterable :: expr ann
  , body :: CodeBlockF stmt expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data WithF stmt ty expr ann = WithF
  { statements :: NonEmptyVector (WithStatementF ty expr ann)
  , block :: CodeBlockF stmt expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data WithStatementF ty expr ann = WithStatementF
  { let_ :: Maybe ann
  , lhs :: WithLhsF ty ann
  , rhs :: WithRhsF ty expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data WithLhsF ty ann
  = WLhsLabelled (SimpleVarIdentifier ann) (Maybe (ty ann))
  | WLhsUnlabelled (ty ann)
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
  { lhs :: ty ann
  , rhs :: SimpleVarIdentifier ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
