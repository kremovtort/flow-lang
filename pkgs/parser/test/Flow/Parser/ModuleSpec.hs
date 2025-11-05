module Flow.Parser.ModuleSpec (spec) where

import "base" Data.Maybe (fromJust)
import "hspec" Test.Hspec (Spec, describe, it)
import "nonempty-vector" Data.Vector.NonEmpty qualified as NE
import "text" Data.Text (Text)
import "text" Data.Text qualified as Text
import "vector" Data.Vector qualified as Vector

import Flow.AST.Surface qualified as Surface
import Flow.AST.Surface.Callable (CallableHeader (whereBlock))
import Flow.AST.Surface.Callable qualified as Surface
import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Constraint qualified as Surface
import Flow.AST.Surface.Decl qualified as Surface
import Flow.AST.Surface.Expr qualified as Surface
import Flow.AST.Surface.Literal qualified as Surface
import Flow.AST.Surface.Module qualified as Surface
import Flow.AST.Surface.Pattern qualified as Surface
import Flow.AST.Surface.Syntax qualified as Surface
import Flow.AST.Surface.Type qualified as Surface
import Flow.AST.Surface.Use qualified as Surface
import Flow.Parser (pModDefinitionBody)
import Flow.Parser.SpecHelpers (shouldBe, shouldBeParsed, testParser)

type ModuleItem =
  Surface.ModuleItemF
    Surface.Mod
    Surface.Statement
    Surface.PatternSimple
    Surface.Type
    Surface.Expression

modIdent :: Text -> Surface.ModuleIdentifier ()
modIdent name = Surface.ModuleIdentifier{name, ann = ()}

simpleVar :: Text -> Surface.SimpleVarIdentifier ()
simpleVar name = Surface.SimpleVarIdentifier{name, ann = ()}

simpleType :: Text -> Surface.Type ()
simpleType name =
  Surface.Type
    { ty =
        Surface.TyIdentifierF
          Surface.AnyTypeIdentifier
            { qualifier = Nothing
            , typeQualifier = Nothing
            , identifier = Surface.SimpleTypeIdentifier{name, ann = ()}
            , ann = ()
            }
    , ann = ()
    }

moduleBody :: [Surface.UseClause ()] -> [ModuleItem ()] -> Surface.ModDefinitionBody ()
moduleBody uses items =
  Surface.ModDefinitionBodyF
    { uses = Vector.fromList uses
    , items = Vector.fromList items
    }

modDecl :: Text -> ModuleItem ()
modDecl name =
  Surface.ModuleItemF
    { pub = Nothing
    , item = Surface.ModItemModF (Surface.Mod (Surface.ModDeclarationF (modIdent name)) ())
    , ann = ()
    }

modDef :: Text -> [Surface.UseClause ()] -> [ModuleItem ()] -> ModuleItem ()
modDef name uses items =
  Surface.ModuleItemF
    { pub = Nothing
    , item =
        Surface.ModItemModF
          ( Surface.Mod
              { mod =
                  Surface.ModDefinitionF
                    (modIdent name)
                    Surface.ModDefinitionBodyF{uses = Vector.fromList uses, items = Vector.fromList items}
              , ann = ()
              }
          )
    , ann = ()
    }

useClauseLeaf :: [Text] -> Surface.UseClause ()
useClauseLeaf path =
  case path of
    [] -> error "empty path"
    root : rest ->
      let
        buildTree [] = Nothing
        buildTree [segment] =
          Just $
            Surface.UseTrLeafVar
              Surface.UseTreeLeaf
                { use = Surface.SimpleVarIdentifier segment ()
                , as = Nothing
                , ann = ()
                }
        buildTree (segment : segments) =
          Surface.UseTrBranch (modIdent segment) <$> buildTree segments
       in
        Surface.UseClause
          { root = modIdent root
          , tree = buildTree rest
          , ann = ()
          }

useClauseAs :: [Text] -> Text -> Surface.UseClause ()
useClauseAs path alias =
  case path of
    [] -> error "empty path"
    root : rest ->
      let
        build [] =
          Just $
            Surface.UseTrLeafVar
              Surface.UseTreeLeaf
                { use = Surface.SimpleVarIdentifier root ()
                , as = Just $ Surface.SimpleVarIdentifier alias ()
                , ann = ()
                }
        build [segment] =
          Just $
            Surface.UseTrLeafVar
              Surface.UseTreeLeaf
                { use = Surface.SimpleVarIdentifier segment ()
                , as = Just $ Surface.SimpleVarIdentifier alias ()
                , ann = ()
                }
        build (segment : segments) = Surface.UseTrBranch (modIdent segment) <$> build segments
       in
        Surface.UseClause
          { root = modIdent root
          , tree = build rest
          , ann = ()
          }

structItem :: Maybe (Surface.Pub ()) -> Text -> ModuleItem ()
structItem pub' name =
  Surface.ModuleItemF
    { pub = pub'
    , item =
        Surface.ModItemStructF
          Surface.StructF
            { name = Surface.SimpleTypeIdentifier{name, ann = ()}
            , typeParams = Nothing
            , fields = Surface.FieldsDeclNamedF mempty
            , ann = ()
            }
    , ann = ()
    }

enumItem :: Maybe (Surface.Pub ()) -> Text -> [Text] -> ModuleItem ()
enumItem pub' name variants =
  Surface.ModuleItemF
    { pub = pub'
    , item =
        Surface.ModItemEnumF
          Surface.EnumF
            { name = Surface.SimpleTypeIdentifier{name, ann = ()}
            , typeParams = Nothing
            , variants =
                Surface.EVariantsSimpleF
                  ( case NE.fromList
                      ( fmap
                          (\vname -> Surface.EnumVariantF{name = Surface.SimpleTypeIdentifier{name = vname, ann = ()}, fields = Nothing, ann = ()})
                          variants
                      ) of
                      Nothing -> error "enumItem: expected non-empty variants"
                      Just ne -> ne
                  )
            , ann = ()
            }
    , ann = ()
    }

typeAliasItem :: Maybe (Surface.Pub ()) -> Text -> Surface.Type () -> ModuleItem ()
typeAliasItem pub' name ty =
  Surface.ModuleItemF
    { pub = pub'
    , item =
        Surface.ModItemTypeAliasF
          Surface.TypeDefinitionF
            { name = Surface.SimpleTypeIdentifier{name, ann = ()}
            , typeParams =
                Just $
                  Surface.BindersF
                    { scopes = mempty
                    , types =
                        Vector.fromList
                          [ Surface.BinderWoConstraintF
                              { name =
                                  Surface.SimpleTypeIdentifier
                                    { name = "X"
                                    , ann = ()
                                    }
                              , kindShort = Nothing
                              , typeType = Nothing
                              , ann = ()
                              }
                          , Surface.BinderWoConstraintF
                              { name =
                                  Surface.SimpleTypeIdentifier
                                    { name = "Y"
                                    , ann = ()
                                    }
                              , kindShort = Nothing
                              , typeType = Nothing
                              , ann = ()
                              }
                          ]
                    , ann = ()
                    }
            , type_ = ty
            , ann = ()
            }
    , ann = ()
    }

fnItem ::
  Maybe (Surface.Pub ()) ->
  Text ->
  [(Bool, Text, Surface.Type ())] ->
  Maybe (Surface.Type ()) ->
  Maybe (Surface.Type ()) ->
  ModuleItem ()
fnItem pub' name args effects result =
  Surface.ModuleItemF
    { pub = pub'
    , item =
        Surface.ModItemFnF
          Surface.CallableF
            { header =
                Surface.CallableHeader
                  { receiver = Surface.UnitF
                  , name = simpleVar name
                  , typeParams = Nothing
                  , args = Vector.fromList (map buildArg args)
                  , effectsResult = do
                      result' <- result
                      pure
                        Surface.FnEffectsResultF
                          { effects = fmap Surface.FnEffectsTypeF effects
                          , result = result'
                          , ann = ()
                          }
                  , whereBlock = Nothing
                  , ann = ()
                  }
            , body =
                Surface.CodeBlockF
                  { region = Nothing
                  , uses = mempty
                  , statements = mempty
                  , result = Nothing
                  , ann = ()
                  }
            , ann = ()
            }
    , ann = ()
    }
 where
  buildArg (mut, name', ty) =
    Surface.ArgF
      { mut = if mut then Just () else Nothing
      , name = simpleVar name'
      , type_ = ty
      , ann = ()
      }

letItem :: Maybe (Surface.Pub ()) -> Text -> Surface.Type () -> Surface.Expression () -> ModuleItem ()
letItem pub' name ty expr =
  Surface.ModuleItemF
    { pub = pub'
    , item =
        Surface.ModItemLetF
          Surface.LetDefinitionF
            { lhs =
                Surface.PatternSimple
                  { patternSimple =
                      Surface.PatSimVarF
                        ( Surface.PatternVariableF
                            { ref = Nothing
                            , mut = Nothing
                            , name = simpleVar name
                            , ann = ()
                            }
                        )
                  , ann = ()
                  }
            , lhsType = Just ty
            , rhs = expr
            , ann = ()
            }
    , ann = ()
    }

tupleType :: [Surface.Type ()] -> Surface.Type ()
tupleType tys = Surface.Type{ty = Surface.TyTupleF (fromJust $ NE.fromList tys), ann = ()}

literalInt :: Integer -> Surface.Expression ()
literalInt n = Surface.Expression{expr = Surface.ELiteral (Surface.LitInteger n), ann = ()}

nonPub :: Maybe (Surface.Pub ())
nonPub = Nothing

pub :: Maybe (Surface.Pub ())
pub = Just Surface.PubPub

spec :: Spec
spec = describe "Module parser (minimal subset)" do
  it "parses mod declaration 'mod m;'" do
    testParser "mod m;" pModDefinitionBody $ shouldBeParsed (`shouldBe` moduleBody [] [modDecl "m"])

  it "parses empty mod definition 'mod m { }'" do
    testParser "mod m { }" pModDefinitionBody $ shouldBeParsed (`shouldBe` moduleBody [] [modDef "m" [] []])

  it "parses use leaf 'use std::io;'" do
    let expected = moduleBody [useClauseLeaf ["std", "io"]] []
    testParser "use std::io;" pModDefinitionBody $ shouldBeParsed (`shouldBe` expected)

  it "parses use leaf-as 'use std::io as io;'" do
    let expected = moduleBody [useClauseAs ["std", "io"] "io"] []
    testParser "use std::io as io;" pModDefinitionBody $ shouldBeParsed (`shouldBe` expected)

  it "parses nested use 'use std::{io, fs::{read, write}};'" do
    let nestedTree =
          Surface.UseTrNested
            ( Vector.fromList
                [ Surface.UseTrLeafVar
                    Surface.UseTreeLeaf
                      { use = Surface.SimpleVarIdentifier "io" ()
                      , as = Nothing
                      , ann = ()
                      }
                , Surface.UseTrBranch
                    (modIdent "fs")
                    ( Surface.UseTrNested $
                        Surface.UseTrLeafVar
                          <$> Vector.fromList
                            [ Surface.UseTreeLeaf
                                { use = Surface.SimpleVarIdentifier "read" ()
                                , as = Nothing
                                , ann = ()
                                }
                            , Surface.UseTreeLeaf
                                { use = Surface.SimpleVarIdentifier "write" ()
                                , as = Nothing
                                , ann = ()
                                }
                            ]
                    )
                ]
            )
        useClause =
          Surface.UseClause
            { root = modIdent "std"
            , tree = Just nestedTree
            , ann = ()
            }
    testParser "use std::{io, fs::{read, write}};" pModDefinitionBody $ shouldBeParsed (`shouldBe` moduleBody [useClause] [])

  it "parses minimal items: struct, enum, type alias, fn, let" do
    let src =
          Text.unlines
            [ "struct S {}"
            , "enum E { A, B }"
            , "type Pair<X, Y> = (X, Y);"
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
              [ (False, "a", Surface.Type{ty = Surface.TyBuiltinF Surface.BuiltinI32, ann = ()})
              , (False, "b", Surface.Type{ty = Surface.TyBuiltinF Surface.BuiltinI32, ann = ()})
              ]
              Nothing
              (Just Surface.Type{ty = Surface.TyBuiltinF Surface.BuiltinI32, ann = ()})
          , letItem nonPub "x" (Surface.Type{ty = Surface.TyBuiltinF Surface.BuiltinI32, ann = ()}) (literalInt 42)
          ]
    testParser src pModDefinitionBody $ shouldBeParsed (`shouldBe` moduleBody [] items)
