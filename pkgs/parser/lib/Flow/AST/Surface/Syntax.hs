module Flow.AST.Surface.Syntax where

import "base" Prelude hiding (Enum)
import "vector" Data.Vector (Vector)

import Flow.AST.Surface.Common (SimpleVarIdentifier, AnyVarIdentifier)
import Data.Vector.NonEmpty (NonEmptyVector)

data UnitF a = UnitF
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- Statements and related nodes
data StatementF lhsExpr pat ty expr ann
  = SLetF (LetDefinitionF pat ty expr ann)
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

data LetDefinitionF pat ty expr ann = LetDefinitionF
  { mutability :: Maybe ann
  , lhs :: pat ann
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

data CodeBlockF lhsExpr pat ty expr ann = CodeBlock
  { statements :: Vector (StatementF lhsExpr pat ty expr ann)
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
