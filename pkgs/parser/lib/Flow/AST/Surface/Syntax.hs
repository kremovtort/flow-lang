module Flow.AST.Surface.Syntax where

import "base" Prelude hiding (Enum)
import "vector" Data.Vector (Vector)

import Flow.AST.Common (SimpleVarIdentifier, AnyVarIdentifier)

data UnitF a = UnitF
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- Statements and related nodes
data StatementF lhsExpr pat ty expr ann
  = SLetF (LetDefinitionF pat ty expr ann)
  | SAssignF (AssignStatementF lhsExpr expr ann)
  | SReturnF (expr ann) ann
  | SContinueF (Maybe (SimpleVarIdentifier ann)) ann
  | SBreakF (Maybe (SimpleVarIdentifier ann)) ann
  | SExpressionF (expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data LetDefinitionF pat ty expr ann = LetDefinition
  { lhs :: pat ann
  , lhsType :: Maybe (ty ann)
  , mutability :: Bool
  , rhs :: expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data AssignStatementF lhsExpr expr ann = AssignStatement
  { lhs :: lhsExpr ann
  , rhs :: expr ann
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
