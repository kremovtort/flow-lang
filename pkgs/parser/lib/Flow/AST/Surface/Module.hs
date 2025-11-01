module Flow.AST.Surface.Module where

import "base" GHC.Generics (Generic)
import "tree-diff" Data.TreeDiff.Class (ToExpr)
import "vector" Data.Vector (Vector)
import "base" Prelude hiding (Enum)

import Flow.AST.Surface.Callable (FnDefinitionF, FnInfixDefinitionF, OpDefinitionF, OpInfixDefinitionF)
import Flow.AST.Surface.Common (ModuleIdentifier, SimpleTypeIdentifier, SimpleVarIdentifier)
import Flow.AST.Surface.Constraint (TypeDefinitionF)
import Flow.AST.Surface.Decl qualified as Decl
import Flow.AST.Surface.Syntax (LetDefinitionF)

data ModF mod stmt simPat pat ty expr ann
  = ModDeclarationF (ModuleIdentifier ann)
  | ModDefinitionF (ModuleIdentifier ann) (ModDefinitionBodyF mod stmt simPat pat ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ModDefinitionBodyF mod stmt simPat pat ty expr ann = ModDefinitionBodyF
  { uses :: Vector (UseClause ann)
  , items :: Vector (ModuleItemF mod stmt simPat pat ty expr ann)
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
  = UseTrBranch (ModuleIdentifier ann) (UseTree ann)
  | UseTrNested (Vector (UseTree ann))
  | UseTrLeafWildcard ann
  | UseTrLeafVar (UseTreeLeaf SimpleVarIdentifier ann)
  | UseTrLeafType (UseTreeLeaf SimpleTypeIdentifier ann)
  | UseTrLeafMethod (UseTreeLeaf SimpleVarIdentifier ann)
  | UseTrLeafMethodAsFn (UseTreeLeafMethodAsFn ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data UseTreeLeaf f ann = UseTreeLeaf
  { use :: f ann
  , as :: Maybe (f ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data UseTreeLeafMethodAsFn ann = UseTreeLeafMethodAsFn
  { use :: SimpleVarIdentifier ann
  , as :: SimpleVarIdentifier ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ModuleItemF mod stmt simPat pat ty expr ann
  = ModuleItemF
  { pub :: Maybe (Decl.Pub ann)
  , item :: ModuleItemVariantF mod stmt simPat ty expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ModuleItemVariantF mod stmt simPat ty expr ann
  = ModItemModF (mod ann)
  | ModItemStructF (Decl.StructF ty ann)
  | ModItemEnumF (Decl.EnumF ty ann)
  | ModItemTraitF (Decl.Trait stmt ty expr ann)
  | ModItemImplF (Decl.Impl stmt simPat ty expr ann)
  | ModItemEffectF (Decl.EffectF stmt ty expr ann)
  | ModItemTypeAliasF (TypeDefinitionF ty ann)
  | ModItemFnF (FnDefinitionF stmt ty expr ann)
  | ModItemFnInfixF (FnInfixDefinitionF stmt ty expr ann)
  | ModItemOpF (OpDefinitionF stmt ty expr ann)
  | ModItemOpInfixF (OpInfixDefinitionF stmt ty expr ann)
  | ModItemLetF (LetDefinitionF simPat ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
