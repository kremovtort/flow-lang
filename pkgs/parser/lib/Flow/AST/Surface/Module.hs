module Flow.AST.Surface.Module where

import "base" Prelude hiding (Enum)
import "vector" Data.Vector (Vector)

import Flow.AST.Common (ModuleIdentifier)
import Flow.AST.Type (TypeDefinition)
import Flow.AST.Surface.Callable.Defs (FnDefinition, FnInfixDefinition, OpDefinition, OpInfixDefinition)
import Flow.AST.Surface.Decl qualified as Decl
import Flow.AST.Surface.Syntax (LetDefinition)

data Mod
  = ModDeclaration ModuleIdentifier
  | ModDefinition ModuleIdentifier ModDefinitionBody
  deriving (Eq, Ord, Show)

data ModDefinitionBody = ModDefinitionBody
  { uses :: Vector UseClause
  , items :: Vector ModuleItem
  }
  deriving (Eq, Ord, Show)

data UseClause = UseClause
  { root :: ModuleIdentifier
  , tree :: UseTree
  }
  deriving (Eq, Ord, Show)

data UseTree
  = UseTreeBranch ModuleIdentifier UseTree
  | UseTreeNested (Vector UseTree)
  | UseTreeLeaf ModuleIdentifier
  | UseTreeLeafAs ModuleIdentifier ModuleIdentifier
  deriving (Eq, Ord, Show)

data ModuleItem
  = ModuleItemMod Mod
  | ModuleItemStruct Decl.Struct
  | ModuleItemEnum Decl.Enum
  | ModuleItemTypeAlias TypeDefinition
  | ModuleItemTrait Decl.Trait
  | ModuleItemImpl Decl.Impl
  | ModuleItemEffect Decl.Effect
  | ModuleItemFn FnDefinition
  | ModuleItemFnInfix FnInfixDefinition
  | ModuleItemOp OpDefinition
  | ModuleItemOpInfix OpInfixDefinition
  | ModuleItemLet LetDefinition
  deriving (Eq, Ord, Show)
