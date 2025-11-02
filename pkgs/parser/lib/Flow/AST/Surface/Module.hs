module Flow.AST.Surface.Module where

import "base" GHC.Generics (Generic)
import "tree-diff" Data.TreeDiff.Class (ToExpr)
import "vector" Data.Vector (Vector)
import "base" Prelude hiding (Enum)

import Flow.AST.Surface.Callable (FnDefinitionF, FnInfixDefinitionF)
import Flow.AST.Surface.Common (ModuleIdentifier, Pub)
import Flow.AST.Surface.Constraint (TypeDefinitionF)
import Flow.AST.Surface.Decl qualified as Decl
import Flow.AST.Surface.Syntax (LetDefinitionF)
import Flow.AST.Surface.Use (UseClause)

data ModF mod stmt simPat pat ty expr ann
  = ModDeclarationF (ModuleIdentifier ann)
  | ModDefinitionF (ModuleIdentifier ann) (ModDefinitionBodyF mod stmt simPat pat ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ModDefinitionBodyF mod stmt simPat pat ty expr ann = ModDefinitionBodyF
  { uses :: Vector (UseClause ann)
  , items :: Vector (ModuleItemF mod stmt simPat pat ty expr ann)
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)


data ModuleItemF mod stmt simPat pat ty expr ann
  = ModuleItemF
  { pub :: Maybe (Pub ann)
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
  | ModItemLetF (LetDefinitionF simPat ty expr ann)
  | ModItemPubUseF (Pub ann) (UseClause ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
