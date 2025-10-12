module Flow.AST.Surface.Module where

import "base" Prelude hiding (Enum)
import "vector" Data.Vector (Vector)

import Flow.AST.Common (ModuleIdentifier)
import Flow.AST.Surface.Constraint (TypeDefinitionF)
import Flow.AST.Surface.Callable (FnDefinitionF, FnInfixDefinitionF, OpDefinitionF, OpInfixDefinitionF)
import Flow.AST.Surface.Decl qualified as Decl
import Flow.AST.Surface.Syntax (LetDefinitionF)

data ModF mod lhsExpr pat ty expr ann
  = ModDeclaration (ModuleIdentifier ann)
  | ModDefinition (ModuleIdentifier ann) (ModDefinitionBodyF mod lhsExpr pat ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data ModDefinitionBodyF mod lhsExpr pat ty expr ann = ModDefinitionBody
  { uses :: Vector (UseClause ann)
  , items :: Vector (ModuleItemF mod lhsExpr pat ty expr ann)
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data UseClause ann = UseClause
  { root :: ModuleIdentifier ann
  , tree :: UseTree ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data UseTree ann
  = UseTreeBranch (ModuleIdentifier ann) (UseTree ann)
  | UseTreeNested (Vector (UseTree ann))
  | UseTreeLeaf (ModuleIdentifier ann)
  | UseTreeLeafAs (ModuleIdentifier ann) (ModuleIdentifier ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data ModuleItemF mod lhsExpr pat ty expr ann
  = ModuleItemModF (mod ann) ann
  | ModuleItemStruct (Decl.StructF ty ann)
  | ModuleItemEnum (Decl.EnumF ty ann)
  | ModuleItemTrait (Decl.Trait lhsExpr pat ty expr ann)
  | ModuleItemImpl (Decl.Impl lhsExpr pat ty expr ann)
  | ModuleItemEffect (Decl.EffectF lhsExpr pat ty expr ann)
  | ModuleItemTypeAlias (TypeDefinitionF ty ann)
  | ModuleItemFn (FnDefinitionF lhsExpr pat ty expr ann)
  | ModuleItemFnInfix (FnInfixDefinitionF lhsExpr pat ty expr ann)
  | ModuleItemOp (OpDefinitionF lhsExpr pat ty expr ann)
  | ModuleItemOpInfix (OpInfixDefinitionF lhsExpr pat ty expr ann)
  | ModuleItemLet (LetDefinitionF pat ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
