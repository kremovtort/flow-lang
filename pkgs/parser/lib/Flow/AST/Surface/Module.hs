module Flow.AST.Surface.Module where

import "base" Prelude hiding (Enum)
import "vector" Data.Vector (Vector)
import "base" GHC.Generics (Generic)
import "tree-diff" Data.TreeDiff.Class (ToExpr)

import Flow.AST.Surface.Common (ModuleIdentifier)
import Flow.AST.Surface.Constraint (TypeDefinitionF)
import Flow.AST.Surface.Callable (FnDefinitionF, FnInfixDefinitionF, OpDefinitionF, OpInfixDefinitionF)
import Flow.AST.Surface.Decl qualified as Decl
import Flow.AST.Surface.Syntax (LetDefinitionF)

data ModF mod lhsExpr simPat pat ty expr ann
  = ModDeclarationF (ModuleIdentifier ann)
  | ModDefinitionF (ModuleIdentifier ann) (ModDefinitionBodyF mod lhsExpr simPat pat ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ModDefinitionBodyF mod lhsExpr simPat pat ty expr ann = ModDefinitionBodyF
  { uses :: Vector (UseClause ann)
  , items :: Vector (ModuleItemF mod lhsExpr simPat pat ty expr ann)
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data UseClause ann = UseClause
  { pub :: Maybe (Decl.Pub ann)
  , root :: ModuleIdentifier ann
  , tree :: Maybe (UseTree ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data UseTree ann
  = UseTreeBranch (ModuleIdentifier ann) (UseTree ann)
  | UseTreeNested (Vector (UseTree ann))
  | UseTreeLeafNamed (ModuleIdentifier ann)
  | UseTreeLeafWildcard ann
  | UseTreeLeafAs (ModuleIdentifier ann) (ModuleIdentifier ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ModuleItemF mod lhsExpr simPat pat ty expr ann
  = ModuleItemF
    { pub :: Maybe (Decl.Pub ann)
    , item :: ModuleItemVariantF mod lhsExpr simPat pat ty expr ann
    , ann :: ann
    }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ModuleItemVariantF mod lhsExpr simPat pat ty expr ann
  = ModItemModF (mod ann) ann
  | ModItemStructF (Decl.StructF ty ann)
  | ModItemEnumF (Decl.EnumF ty ann)
  | ModItemTraitF (Decl.Trait lhsExpr simPat pat ty expr ann)
  | ModItemImplF (Decl.Impl lhsExpr simPat pat ty expr ann)
  | ModItemEffectF (Decl.EffectF lhsExpr simPat pat ty expr ann)
  | ModItemTypeAliasF (TypeDefinitionF ty ann)
  | ModItemFnF (FnDefinitionF lhsExpr simPat pat ty expr ann)
  | ModItemFnInfixF (FnInfixDefinitionF lhsExpr simPat pat ty expr ann)
  | ModItemOpF (OpDefinitionF lhsExpr simPat pat ty expr ann)
  | ModItemOpInfixF (OpInfixDefinitionF lhsExpr simPat pat ty expr ann)
  | ModItemLetF (LetDefinitionF simPat ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
