module Flow.AST.Surface.Syntax where

import "base" GHC.Generics (Generic)
import "tree-diff" Data.TreeDiff.Class (ToExpr)
import "vector" Data.Vector (Vector)
import "base" Prelude hiding (Enum)

import Data.Vector.NonEmpty (NonEmptyVector)
import Flow.AST.Surface.Common (SimpleVarIdentifier)

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
  | SWhileF (WhileExpressionF stmt pat expr ann)
  | SForF (ForExpressionF pat expr ann)
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
  { statements :: Vector (stmt ann)
  , result :: Maybe (expr ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

-- Match and control flow

data MatchExpressionF pat expr ann = MatchExpressionF
  { value :: expr ann
  , arms :: NonEmptyVector (MatchArmF pat expr ann)
  , armsAnn :: ann
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

data LetConditionF pat expr ann = ConditionF
  { pattern :: pat ann
  , patternExpr :: expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data LoopExpressionF stmt expr ann = LoopExpressionF
  { label :: Maybe (SimpleVarIdentifier ann)
  , body :: CodeBlockF stmt expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data WhileExpressionF stmt pat expr ann = WhileExpressionF
  { label :: Maybe (SimpleVarIdentifier ann)
  , condition :: ConditionF pat expr ann
  , body :: CodeBlockF stmt expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ForExpressionF pat expr ann = ForExpressionF
  { label :: Maybe (SimpleVarIdentifier ann, ann)
  , pattern :: pat ann
  , iterable :: expr ann
  , body :: expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

{-
- Fields syntax:
  struct X { a: i32, b?: string }
  let x = X { a = 1, b ?= "hello" }; // direct construction, b wrapped in Some
  { let b = "hello"; let x = X { a = 1, b? }; } // optional field punning
  let x = X { a = 1, b = Some("hello") };
  { let b = Some("hello"); let x = X { a = 1, b }; } // field punning

- Fields syntax in function arguments:
    fn f(a: i32, b?: string)
  - For unnamed arguments call:
      f(1, "hello"?);
      f(1, Some("hello"));
  - For named arguments call:
      f { a = 1, b ?= "hello" };
      f { a = 1, b = Some("hello") };
      { let b = "hello"; f { a = 1, b? }; }
      { let b = Some("hello"); f { a = 1, b }; }
-}
