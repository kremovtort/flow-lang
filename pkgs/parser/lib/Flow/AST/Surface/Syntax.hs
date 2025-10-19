module Flow.AST.Surface.Syntax where

import "base" Prelude hiding (Enum)
import "vector" Data.Vector (Vector)

import Flow.AST.Surface.Common (SimpleVarIdentifier, AnyVarIdentifier, AnyTypeIdentifier)
import Data.Vector.NonEmpty (NonEmptyVector)

data UnitF a = UnitF
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- Statements and related nodes
data StatementF lhsExpr simPat pat ty expr ann
  = SLetF (LetDefinitionF simPat ty expr ann)
  | SAssignF (AssignStatementF lhsExpr expr ann)
  | SReturnF (expr ann) ann
  | SContinueF (Maybe (SimpleVarIdentifier ann)) ann
  | SBreakF (Maybe (SimpleVarIdentifier ann)) ann
  | SMatchF (MatchExpression pat expr ann) ann
  | SIfF (IfExpression expr ann) ann
  | SLoopF (LoopExpression expr ann) ann
  | SWhileF (WhileExpression expr ann) ann
  | SForF (ForExpression pat expr ann) ann
  | SExpressionF (expr ann) ann
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data LetDefinitionF simPat ty expr ann = LetDefinitionF
  { lhs :: simPat ann
  , lhsAnn :: ann
  , lhsType :: Maybe (ty ann, ann)
  , rhs :: expr ann
  , rhsAnn :: ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data AssignStatementF lhsExpr expr ann = AssignStatementF
  { lhs :: lhsExpr ann
  , lhsAnn :: ann
  , rhs :: expr ann
  , rhsAnn :: ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data LHSExpressionF lhsExpr expr ann
  = LHSEWildcard ann
  | LHSEVar (SimpleVarIdentifier ann)
  | LHSEIndex (lhsExpr ann) (expr ann) ann
  | LHSEDotAccess (lhsExpr ann) (AnyVarIdentifier ann) ann
  | LHSEUnOp (LHSUnOpExpression expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype LHSUnOpExpression expr ann
  = LHSUnOpExpressionDeref (expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data CodeBlockF lhsExpr simPat pat ty expr ann = CodeBlock
  { statements :: Vector (StatementF lhsExpr simPat pat ty expr ann)
  , result :: Maybe (expr ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- Match and control flow

data MatchExpression pat expr ann = MatchExpression
  { value :: expr ann
  , valueAnn :: ann
  , arms :: NonEmptyVector (MatchArm pat expr ann, ann)
  , armsAnn :: ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data MatchArm pat expr ann = MatchArm
  { pattern :: pat ann
  , patternAnn :: ann
  , guard :: Maybe (expr ann, ann)
  , expression :: expr ann
  , expressionAnn :: ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data IfExpression expr ann = IfExpression
  { condition :: expr ann
  , conditionAnn :: ann
  , then_ :: expr ann
  , thenAnn :: ann
  , elseIfs :: Vector (expr ann, expr ann, ann)
  , else_ :: Maybe (expr ann, ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data LoopExpression expr ann = LoopExpression
  { label :: Maybe (SimpleVarIdentifier ann, ann)
  , body :: expr ann
  , bodyAnn :: ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data WhileExpression expr ann = WhileExpression
  { label :: Maybe (SimpleVarIdentifier ann, ann)
  , condition :: expr ann
  , conditionAnn :: ann
  , body :: expr ann
  , bodyAnn :: ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data ForExpression pat expr ann = ForExpression
  { label :: Maybe (SimpleVarIdentifier ann, ann)
  , pattern :: pat ann
  , patternAnn :: ann
  , iterable :: expr ann
  , iterableAnn :: ann
  , body :: expr ann
  , bodyAnn :: ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data ConstructorAppF a ty ann = ConstructorAppF
  { name :: AnyTypeIdentifier ann
  , params :: Maybe (Vector (ty ann), ann)
  , fields :: Maybe (Fields a ann, ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Fields inner ann
  = FieldsTuple (Vector (inner ann, ann))
  -- TODO: add punning fields syntax e.g. Cons { head, tail }
  | FieldsNamed (Vector (SimpleVarIdentifier ann, inner ann, ann))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

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

data FieldNamedF inner ann = FieldNamedF
  { name :: SimpleVarIdentifier ann
  , optional :: Maybe ann
  , value :: Maybe (inner ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
