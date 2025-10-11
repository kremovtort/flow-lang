{-# LANGUAGE NoImplicitPrelude #-}

module Flow.AST where

import "base" Prelude hiding (Enum)

import "base" Data.Word (Word8)
import "bytestring" Data.ByteString (ByteString)
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "scientific" Data.Scientific (Scientific)
import "text" Data.Text (Text)
import "vector" Data.Vector (Vector)

import Flow.AST.Common (
  AnyTypeIdentifier,
  AnyVarIdentifier,
  ModuleIdentifier,
  Scope,
  SimpleTypeIdentifier,
  SimpleVarIdentifier,
 )
import Flow.AST.Type (Type)
import Flow.AST.Type qualified as Type

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
  | ModuleItemStruct Struct
  | ModuleItemEnum Enum
  | ModuleItemTypeAlias Type.TypeDefinition
  | ModuleItemTrait Trait
  | ModuleItemImpl Impl
  | ModuleItemEffect Effect
  | ModuleItemFn FnDefinition
  | ModuleItemFnInfix FnInfixDefinition
  | ModuleItemLet LetDefinition
  deriving (Eq, Ord, Show)

data Struct = Struct
  { name :: SimpleTypeIdentifier
  , scopeParams :: Vector Scope
  , typeParams :: Vector Type.BinderWoConstraint
  , fields :: Vector (Fields Type)
  }
  deriving (Eq, Ord, Show)

data Enum = Enum
  { name :: SimpleTypeIdentifier
  , scopeParams :: Vector Scope
  , typeParams :: Vector Type.BinderWoConstraint
  , variants :: EnumVariants
  }
  deriving (Eq, Ord, Show)

data EnumVariants
  = EVariantsSimple (NonEmptyVector EnumVariant)
  | EVariantsGeneralized (NonEmptyVector EnumVariantGeneralized)
  deriving (Eq, Ord, Show)

data EnumVariant = EnumVariant
  { name :: SimpleTypeIdentifier
  , enumFields :: Maybe (Fields Type)
  }
  deriving (Eq, Ord, Show)

data Fields v
  = FieldsTuple (NonEmptyVector v)
  | FieldsNamed (NonEmptyVector (SimpleVarIdentifier, v))
  deriving (Eq, Ord, Show)

data EnumVariantGeneralized
  = EVariantGeneralizedSimple EnumVariantGeneralizedSimple
  | EVariantGeneralizedForall (Type.Forall EnumVariantGeneralizedSimple)
  deriving (Eq, Ord, Show)

data EnumVariantGeneralizedSimple = EnumVariantGeneralizedSimple
  { enumVariant :: EnumVariant
  , type_ :: Type
  }
  deriving (Eq, Ord, Show)

data Trait = Trait
  { name :: SimpleTypeIdentifier
  , scopeParams :: Vector Scope
  , typeParams :: Vector Type.BinderWoConstraint
  , superTraits :: Vector Type
  , traitBody :: Vector TraitItem
  }
  deriving (Eq, Ord, Show)

data TraitItem
  = TItemTypeDeclaration TypeDeclaration
  | TItemLetDeclaration LetDeclaration
  | TItemFnDeclaration FnDeclaration
  | TItemOpDeclaration FnInfixDeclaration
  deriving (Eq, Ord, Show)

data Impl = Impl
  { scopeParams :: Vector Scope
  , typeParams :: Vector Type.Binder
  , trait :: Type
  , whereClauses :: Vector Type.WhereClause
  , body :: Vector ImplItem
  }
  deriving (Eq, Ord, Show)

data ImplItem
  = ImplItemType Type.TypeDefinition
  | ImplItemLet LetDefinition
  | ImplItemFn FnDefinition
  | ImplItemFnInfix FnInfixDefinition
  deriving (Eq, Ord, Show)

data Effect = Effect
  { name :: SimpleTypeIdentifier
  , scopeParams :: Vector Scope
  , typeParams :: Vector Type.Binder
  , superEffects :: Vector Type
  , whereClauses :: Vector Type.WhereClause
  , effectContent :: Vector EffectItemDeclaration
  }
  deriving (Eq, Ord, Show)

data FnDefinition = FnDefinition
  { name :: SimpleVarIdentifier
  , scopeParams :: Vector Scope
  , typeParams :: Vector Type.Binder
  , args :: Vector (SimpleVarIdentifier, Type)
  , result :: Type
  , whereClauses :: Vector Type.WhereClause
  , body :: BlockExpression
  }
  deriving (Eq, Ord, Show)

data FnInfixDefinition = FnInfixDefinition
  { calleeScopeParams :: Vector Scope
  , calleeTypeParams :: Vector Type.Binder
  , calleeName :: SimpleVarIdentifier
  , calleeType :: Type
  , name :: SimpleVarIdentifier
  , scopeParams :: Vector Scope
  , typeParams :: Vector Type.Binder
  , args :: Vector (SimpleVarIdentifier, Type)
  , result :: Type
  , whereClauses :: Vector Type.WhereClause
  , body :: BlockExpression
  }
  deriving (Eq, Ord, Show)

data OpDefinition = OpDefinition
  { calleeScopeParams :: Vector Scope
  , calleeTypeParams :: Vector Type.Binder
  , calleeName :: SimpleVarIdentifier
  , calleeType :: Type
  , name :: SimpleVarIdentifier
  , scopeParams :: Vector Scope
  , typeParams :: Vector Type.Binder
  , args :: Vector (SimpleVarIdentifier, Type)
  , result :: Type
  , whereClauses :: Vector Type.WhereClause
  , body :: BlockExpression
  }
  deriving (Eq, Ord, Show)

data OpInfixDefinition = OpInfixDefinition
  { calleeScopeParams :: Vector Scope
  , calleeTypeParams :: Vector Type.Binder
  , calleeName :: SimpleVarIdentifier
  , calleeType :: Type
  , name :: SimpleVarIdentifier
  , scopeParams :: Vector Scope
  , typeParams :: Vector Type.Binder
  , args :: Vector (SimpleVarIdentifier, Type)
  , result :: Type
  , whereClauses :: Vector Type.WhereClause
  , body :: BlockExpression
  }
  deriving (Eq, Ord, Show)

data EffectItemDeclaration
  = EItemLetDeclaration LetDeclaration
  | EItemOpDeclaration OpDeclaration
  | EItemOpInfixDeclaration OpInfixDeclaration
  deriving (Eq, Ord, Show)

data TypeDeclaration = TypeDeclaration
  { name :: SimpleTypeIdentifier
  , type_ :: Type
  }
  deriving (Eq, Ord, Show)

data LetDeclaration = LetDeclaration
  { name :: SimpleVarIdentifier
  , type_ :: Type
  }
  deriving (Eq, Ord, Show)

data FnDeclaration = FnDeclaration
  { name :: SimpleVarIdentifier
  , scopeParams :: Vector Scope
  , typeParams :: Vector Type.Binder
  , args :: Vector (SimpleVarIdentifier, Type)
  , result :: Type
  , whereClauses :: Vector Type.WhereClause
  }
  deriving (Eq, Ord, Show)

data FnInfixDeclaration = FnInfixDeclaration
  { calleeScopeParams :: Vector Scope
  , calleeTypeParams :: Vector Type.Binder
  , calleeName :: SimpleVarIdentifier
  , calleeType :: Type
  , name :: SimpleVarIdentifier
  , scopeParams :: Vector Scope
  , typeParams :: Vector Type.Binder
  , args :: Vector (SimpleVarIdentifier, Type)
  , result :: Type
  , whereClauses :: Vector Type.WhereClause
  }
  deriving (Eq, Ord, Show)

data OpDeclaration = OpDeclaration
  { name :: SimpleVarIdentifier
  , scopeParams :: Vector Scope
  , typeParams :: Vector Type.Binder
  , args :: Vector (SimpleVarIdentifier, Type)
  , result :: Type
  , whereClauses :: Vector Type.WhereClause
  }
  deriving (Eq, Ord, Show)

data OpInfixDeclaration = OpInfixDeclaration
  { calleeScopeParams :: Vector Scope
  , calleeTypeParams :: Vector Type.Binder
  , calleeName :: SimpleVarIdentifier
  , calleeType :: Type
  , name :: SimpleVarIdentifier
  , scopeParams :: Vector Scope
  , typeParams :: Vector Type.Binder
  , args :: Vector (SimpleVarIdentifier, Type)
  , result :: Type
  , whereClauses :: Vector Type.WhereClause
  }
  deriving (Eq, Ord, Show)

data Statement
  = SLet LetDefinition
  | SReturn Expression
  | SContinue (Maybe SimpleVarIdentifier)
  | SBreak (Maybe SimpleVarIdentifier)
  | SExpression Expression
  deriving (Eq, Ord, Show)

data LetDefinition = LetDefinition
  { lhs :: Pattern
  , lhsType :: Maybe Type
  , mutability :: Bool
  , rhs :: Expression
  }
  deriving (Eq, Ord, Show)

data Pattern
  = PatternWildcard -- _
  | PatternLiteral Literal -- 0 | true | "str"
  | PatternVar SimpleVarIdentifier -- x
  | PatternTuple (NonEmptyVector Pattern) -- (a, b, c)
  | PatternCons (ConstructorApp Pattern) -- EnumVariant | Some(1) | Cons { a = 1, b = 2 }
  | PatternOr (NonEmptyVector Pattern) -- a | b | c
  deriving (Eq, Ord, Show)

data Literal
  = LitBool Bool
  | LitInteger Integer
  | LitFloat Scientific
  | LitByte Word8
  | LitByteString ByteString
  | LitChar Char
  | LitString Text
  deriving (Eq, Ord, Show)

data Expression
  = EWildcard -- _
  | ELiteral Literal -- 0 | true | "str"
  | EOfType Expression Type -- expr : T
  | EVar AnyVarIdentifier -- ident | someModule::ident
  | EDo Expression -- expr.do
  | EDotAccess Expression AnyVarIdentifier -- expr.ident
  | EFnCall FnCall -- f(a, b) | f(arg1 = a, arg2 = b) | f<T>(a, b) | f() with { State<S> = e }
  | EUnop UnopExpression -- !a | -a | *a | &a | &mut a | &'s mut a
  | EBinop BinopExpression -- a * b | a + b | a ++ b | etc
  | EConstructorAsFn AnyTypeIdentifier -- EnumVariant | Some | Cons
  | EConstructorApp (ConstructorApp Expression) -- EnumVariant | Some(1) | Cons { a = 1, b = 2 }
  | ETuple (NonEmptyVector Expression) -- (a, b, c)
  | EMatch MatchExpression -- match expr { Pattern => expr, ... }
  | EIf IfExpression -- if expr { then_ } else { else_ }
  | ELoop LoopExpression -- loop { ... } | 'label: loop { ...; break 'label; ... }
  | EWhile WhileExpression -- while expr { ... } | 'label: while expr { ...; break 'label; ... }
  | EFor ForExpression -- for pattern in iterable { ... }
  -- 'label: for pattern in iterable { ...; break 'label; ... }
  | EBlock BlockExpression -- { ... }
  | EHandle HandleExpression -- handle Effect
  | ELambda LambdaExpression -- <A>|a: T, b: T| -> T where { Monoid<T> } { a ++ B }
  | EForall ForallExpression -- <A> Monoid::append<A> where { Monoid<A> }
  -- надо подумать как лучше реализовать параметризацию companion модулей типами
  -- похоже не обойтись без дефолтных реализаций для трейтов и эффектов
  deriving (Eq, Ord, Show)

data HandleExpression
  = HSingle HandleSingleExpression
  | HMultiple HandleMultipleExpression
  deriving (Eq, Ord, Show)

data HandleSingleExpression = HandleSingleExpression
  { effect :: Type
  , in_ :: Maybe Type.EffectRow
  , returning :: Maybe HandleReturning
  , body :: NonEmptyVector EffectItemDefinition
  }
  deriving (Eq, Ord, Show)

data HandleMultipleExpression = HandleMultipleExpression
  { in_ :: Maybe Type.EffectRow
  , returning :: Maybe HandleReturning
  , body :: NonEmptyVector HandleMultipleExpressionItem
  }
  deriving (Eq, Ord, Show)

data HandleMultipleExpressionItem = HandleMultipleExpressionItem
  { effect :: Type
  , returning :: Maybe HandleReturning
  , body :: NonEmptyVector EffectItemDefinition
  }
  deriving (Eq, Ord, Show)

data EffectItemDefinition
  = EDefinitionLet LetDefinition
  | EDefinitionOp OpDefinition
  | EDefinitionOpInfix OpInfixDefinition
  deriving (Eq, Ord, Show)

data HandleReturning = HandleReturning
  { binder :: Type.BinderWoConstraint
  , result :: Type
  }
  deriving (Eq, Ord, Show)

data FnCall = FnCall
  { callee :: Expression
  , scopeParams :: Maybe (NonEmptyVector Scope)
  , typeParams :: Maybe (NonEmptyVector Type)
  , args :: Vector (Fields Expression)
  , withEffects :: Maybe (Vector WithEffectsItem)
  }
  deriving (Eq, Ord, Show)

data WithEffectsItem = WithEffectsItem
  { lhs :: Either SimpleVarIdentifier Type
  , rhs :: Either SimpleVarIdentifier Type
  }
  deriving (Eq, Ord, Show)

data ConstructorApp t = ConstructorApp
  { name :: AnyTypeIdentifier
  , fields :: Fields t
  }
  deriving (Eq, Ord, Show)

data MatchExpression = MatchExpression
  { value :: Expression
  , arms :: NonEmptyVector MatchArm
  }
  deriving (Eq, Ord, Show)

data MatchArm = MatchArm
  { pattern :: Pattern
  , guard :: Maybe Expression
  , expression :: Expression
  }
  deriving (Eq, Ord, Show)

data IfExpression = IfExpression
  { condition :: Expression
  , then_ :: Expression
  , else_ :: Maybe Expression
  }
  deriving (Eq, Ord, Show)

data LoopExpression = LoopExpression
  { label :: Maybe SimpleVarIdentifier
  , body :: Expression
  }
  deriving (Eq, Ord, Show)

data WhileExpression = WhileExpression
  { label :: Maybe SimpleVarIdentifier
  , condition :: Expression
  , body :: Expression
  }
  deriving (Eq, Ord, Show)

data ForExpression = ForExpression
  { label :: Maybe SimpleVarIdentifier
  , pattern :: Pattern
  , iterable :: Expression
  , body :: Expression
  }
  deriving (Eq, Ord, Show)

newtype BlockExpression = BlockExpression
  { statements :: NonEmptyVector Statement
  }
  deriving (Eq, Ord, Show)
