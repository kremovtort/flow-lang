module Flow.Parser.ModuleSpec (spec) where

import "base" Data.Maybe (fromJust)
import "hspec" Test.Hspec (Spec, describe, it)
import "nonempty-vector" Data.Vector.NonEmpty qualified as NE
import "text" Data.Text (Text)
import "text" Data.Text qualified as Text
import "vector" Data.Vector qualified as Vector

import Flow.AST.Surface qualified as Surface
import Flow.AST.Surface.Callable (CallableHeader (whereBlock))
import Flow.AST.Surface.Callable qualified as Callable
import Flow.AST.Surface.Common qualified as C
import Flow.AST.Surface.Constraint qualified as Constraint
import Flow.AST.Surface.Decl qualified as Decl
import Flow.AST.Surface.Expr qualified as Expr
import Flow.AST.Surface.Literal qualified as Lit
import Flow.AST.Surface.Module qualified as M
import Flow.AST.Surface.Pattern qualified as Pat
import Flow.AST.Surface.Syntax qualified as Syn
import Flow.AST.Surface.Type qualified as Ty
import Flow.Parser.Helpers (testParser)
import Flow.Parser.Module qualified as PMod

type ModuleItem = M.ModuleItemF Surface.Mod Surface.LHSExpression Surface.PatternSimple Surface.Pattern Surface.Type Surface.Expression

modIdent :: Text -> C.ModuleIdentifier ()
modIdent name = C.ModuleIdentifier{name, ann = ()}

simpleVar :: Text -> C.SimpleVarIdentifier ()
simpleVar name = C.SimpleVarIdentifier{name, ann = ()}

simpleType :: Text -> Surface.Type ()
simpleType name =
  Surface.Type
    { ty =
        Ty.TyIdentifierF
          C.AnyTypeIdentifier
            { qualifier = Vector.empty
            , qualifierAnn = Nothing
            , identifier = C.SimpleTypeIdentifier{name, ann = ()}
            , identifierAnn = ()
            , ann = ()
            }
    , ann = ()
    }

moduleBody :: [M.UseClause ()] -> [ModuleItem ()] -> Surface.ModDefinitionBody ()
moduleBody uses items =
  M.ModDefinitionBodyF
    { uses = Vector.fromList uses
    , items = Vector.fromList items
    }

modDecl :: Text -> ModuleItem ()
modDecl name =
  M.ModuleItemF
    { pub = Nothing
    , item = M.ModItemModF (Surface.Mod (M.ModDeclarationF (modIdent name)) ()) ()
    , ann = ()
    }

modDef :: Text -> [M.UseClause ()] -> [ModuleItem ()] -> ModuleItem ()
modDef name uses items =
  M.ModuleItemF
    { pub = Nothing
    , item =
        M.ModItemModF
          ( Surface.Mod
              { mod =
                  M.ModDefinitionF
                    (modIdent name)
                    M.ModDefinitionBodyF{uses = Vector.fromList uses, items = Vector.fromList items}
              , ann = ()
              }
          )
          ()
    , ann = ()
    }

useClauseLeaf :: [Text] -> M.UseClause ()
useClauseLeaf path =
  case path of
    [] -> error "empty path"
    root : rest ->
      let
        buildTree [] = Nothing
        buildTree [segment] = Just $ M.UseTreeLeafNamed (modIdent segment)
        buildTree (segment : segments) =
          M.UseTreeBranch (modIdent segment) <$> buildTree segments
       in
        M.UseClause
          { pub = Nothing
          , root = modIdent root
          , tree = buildTree rest
          , ann = ()
          }

useClauseAs :: [Text] -> Text -> M.UseClause ()
useClauseAs path alias =
  case path of
    [] -> error "empty path"
    root : rest ->
      let
        build [] = Just $ M.UseTreeLeafAs (modIdent root) (modIdent alias)
        build [segment] = Just $ M.UseTreeLeafAs (modIdent segment) (modIdent alias)
        build (segment : segments) = M.UseTreeBranch (modIdent segment) <$> build segments
       in
        M.UseClause
          { pub = Nothing
          , root = modIdent root
          , tree = build rest
          , ann = ()
          }

structItem :: Maybe (Decl.Pub ()) -> Text -> ModuleItem ()
structItem pub' name =
  M.ModuleItemF
    { pub = pub'
    , item =
        M.ModItemStructF
          Decl.StructF
            { name = C.SimpleTypeIdentifier{name, ann = ()}
            , typeParams = Nothing
            , fields = mempty
            , fieldsAnn = ()
            , ann = ()
            }
    , ann = ()
    }

enumItem :: Maybe (Decl.Pub ()) -> Text -> [Text] -> ModuleItem ()
enumItem pub' name variants =
  M.ModuleItemF
    { pub = pub'
    , item =
        M.ModItemEnumF
          Decl.EnumF
            { name = C.SimpleTypeIdentifier{name, ann = ()}
            , typeParams = Nothing
            , variants =
                Decl.EVariantsSimpleF
                  ( case NE.fromList
                      ( fmap
                          (\vname -> Decl.EnumVariantF{name = C.SimpleTypeIdentifier{name = vname, ann = ()}, fields = Nothing, ann = ()})
                          variants
                      ) of
                      Nothing -> error "enumItem: expected non-empty variants"
                      Just ne -> ne
                  )
            , ann = ()
            , variantsAnn = ()
            }
    , ann = ()
    }

typeAliasItem :: Maybe (Decl.Pub ()) -> Text -> Surface.Type () -> ModuleItem ()
typeAliasItem pub' name ty =
  M.ModuleItemF
    { pub = pub'
    , item =
        M.ModItemTypeAliasF
          Constraint.TypeDefinitionF
            { name = C.SimpleTypeIdentifier{name, ann = ()}
            , scopeParams = mempty
            , typeParams =
                Vector.fromList
                  [ Constraint.BinderWoConstraintF{name = C.SimpleTypeIdentifier{name = "X", ann = ()}, typeType = Nothing, ann = ()}
                  , Constraint.BinderWoConstraintF{name = C.SimpleTypeIdentifier{name = "Y", ann = ()}, typeType = Nothing, ann = ()}
                  ]
            , type_ = ty
            , ann = ()
            }
    , ann = ()
    }

fnItem ::
  Maybe (Decl.Pub ()) ->
  Text ->
  [(Bool, Text, Surface.Type ())] ->
  Maybe (Surface.Type ()) ->
  Maybe (Surface.Type ()) ->
  ModuleItem ()
fnItem pub' name args effects result =
  M.ModuleItemF
    { pub = pub'
    , item =
        M.ModItemFnF
          Callable.CallableF
            { header =
                Callable.CallableHeader
                  { receiver = Syn.UnitF
                  , name = simpleVar name
                  , typeParams = Nothing
                  , argsRequired = Vector.fromList (map buildArg args)
                  , argsRequiredAnn = ()
                  , argsOptional = mempty
                  , argsOptionalAnn = ()
                  , effects = (,()) <$> effects
                  , result = (,()) <$> result
                  , whereBlock = Nothing
                  }
            , body = Syn.CodeBlock{statements = mempty, result = Nothing, ann = ()}
            }
    , ann = ()
    }
 where
  buildArg (mut, name', ty) =
    Callable.ArgF
      { mut = if mut then Just () else Nothing
      , name = simpleVar name'
      , type_ = ty
      , ann = ()
      }

letItem :: Maybe (Decl.Pub ()) -> Text -> Surface.Type () -> Surface.Expression () -> ModuleItem ()
letItem pub' name ty expr =
  M.ModuleItemF
    { pub = pub'
    , item =
        M.ModItemLetF
          Syn.LetDefinitionF
            { lhs =
                Surface.PatternSimple
                  { patternSimple =
                      Pat.PatSimVarF
                        ( Pat.PatternVariableF
                            { mut = Nothing
                            , name = simpleVar name
                            , ann = ()
                            }
                        )
                  , ann = ()
                  }
            , lhsAnn = ()
            , lhsType = Just (ty, ())
            , rhs = expr
            , rhsAnn = ()
            , ann = ()
            }
    , ann = ()
    }

tupleType :: [Surface.Type ()] -> Surface.Type ()
tupleType tys = Surface.Type{ty = Ty.TyTupleF (fromJust $ NE.fromList tys) (), ann = ()}

literalInt :: Integer -> Surface.Expression ()
literalInt n = Surface.Expression{expr = Expr.ELiteral (Lit.LitInteger n), ann = ()}

nonPub :: Maybe (Decl.Pub ())
nonPub = Nothing

pub :: Maybe (Decl.Pub ())
pub = Just Decl.PubPub

rootModuleName :: Text
rootModuleName = "_"

spec :: Spec
spec = describe "Module parser (minimal subset)" do
  it "parses mod declaration 'mod m;'" do
    testParser "mod m;" PMod.pModDefinitionBody (Just (moduleBody [] [modDecl "m"]))

  it "parses empty mod definition 'mod m { }'" do
    testParser "mod m { }" PMod.pModDefinitionBody (Just (moduleBody [] [modDef "m" [] []]))

  it "parses use leaf 'use std::io;'" do
    let expected = moduleBody [useClauseLeaf ["std", "io"]] []
    testParser "use std::io;" PMod.pModDefinitionBody (Just expected)

  it "parses use leaf-as 'use std::io as IO;'" do
    let expected = moduleBody [useClauseAs ["std", "io"] "IO"] []
    testParser "use std::io as IO;" PMod.pModDefinitionBody (Just expected)

  it "parses nested use 'use std::{io, fs::{read, write}};'" do
    let nestedTree =
          M.UseTreeNested
            ( Vector.fromList
                [ M.UseTreeLeafNamed (modIdent "io")
                , M.UseTreeBranch
                    (modIdent "fs")
                    ( M.UseTreeNested
                        ( Vector.fromList
                            [ M.UseTreeLeafNamed (modIdent "read")
                            , M.UseTreeLeafNamed (modIdent "write")
                            ]
                        )
                    )
                ]
            )
        useClause =
          M.UseClause
            { pub = Nothing
            , root = modIdent "std"
            , tree = Just nestedTree
            , ann = ()
            }
    testParser "use std::{io, fs::{read, write}};" PMod.pModDefinitionBody (Just (moduleBody [useClause] []))

  it "parses minimal items: struct, enum, type alias, fn, let" do
    let src =
          Text.unlines
            [ "struct S {}"
            , "enum E { A, B }"
            , "type Pair<X, Y> = (X, Y)"
            , "fn add(a: i32, b: i32) -> i32 { }"
            , "let x: i32 = 42;"
            ]
        items =
          [ structItem nonPub "S"
          , enumItem nonPub "E" ["A", "B"]
          , typeAliasItem nonPub "Pair" (tupleType [simpleType "X", simpleType "Y"])
          , fnItem
              nonPub
              "add"
              [ (False, "a", Surface.Type{ty = Ty.TyBuiltinF Ty.BuiltinI32 (), ann = ()})
              , (False, "b", Surface.Type{ty = Ty.TyBuiltinF Ty.BuiltinI32 (), ann = ()})
              ]
              Nothing
              (Just Surface.Type{ty = Ty.TyBuiltinF Ty.BuiltinI32 (), ann = ()})
          , letItem nonPub "x" (Surface.Type{ty = Ty.TyBuiltinF Ty.BuiltinI32 (), ann = ()}) (literalInt 42)
          ]
    testParser src PMod.pModDefinitionBody (Just (moduleBody [] items))
