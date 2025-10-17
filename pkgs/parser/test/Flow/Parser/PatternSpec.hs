module Flow.Parser.PatternSpec (spec) where

import "base" Data.Bifunctor qualified as Bifunctor
import "base" Data.List.NonEmpty qualified as ListNE
import "hspec" Test.Hspec (Spec, describe, it)
import "nonempty-vector" Data.Vector.NonEmpty qualified as NE
import "text" Data.Text (Text)
import "vector" Data.Vector qualified as Vector

import Flow.AST.Surface qualified as Surface
import Flow.AST.Surface.Common qualified as C
import Flow.AST.Surface.Literal qualified as Lit
import Flow.AST.Surface.Pattern qualified as Pat
import Flow.AST.Surface.Syntax qualified as Syn
import Flow.Parser.Helpers (testParser)
import Flow.Parser.Pattern qualified as PPat

anyType :: C.SimpleTypeIdentifier () -> C.AnyTypeIdentifier ()
anyType ident =
  C.AnyTypeIdentifier
    { qualifier = Vector.empty
    , qualifierAnn = Nothing
    , identifier = ident
    , identifierAnn = ()
    , ann = ()
    }

mkVar :: Text -> C.SimpleVarIdentifier ()
mkVar name = C.SimpleVarIdentifier{name, ann = ()}

wrapSimple :: Pat.PatternSimpleF Surface.Pattern Surface.Type () -> Surface.Pattern ()
wrapSimple simple =
  Surface.Pattern
    { pattern = Pat.PatternSimpleF simple ()
    , ann = ()
    }

wildcardPattern :: Surface.Pattern ()
wildcardPattern = wrapSimple (Pat.PatternSimpleWildcardF ())

literalBoolPattern :: Bool -> Surface.Pattern ()
literalBoolPattern value =
  wrapSimple (Pat.PatternSimpleLiteralF (Lit.LitBool value ()) ())

literalIntPattern :: Integer -> Surface.Pattern ()
literalIntPattern value =
  wrapSimple (Pat.PatternSimpleLiteralF (Lit.LitInteger value ()) ())

varPattern :: Text -> Surface.Pattern ()
varPattern name = wrapSimple (Pat.PatternSimpleVarF (mkVar name) ())

tuplePattern :: [Surface.Pattern ()] -> Surface.Pattern ()
tuplePattern ps = wrapSimple (Pat.PatternSimpleTupleF (requireVector "tuplePattern" ps) ())

constructorPattern :: Text -> Maybe [Surface.Type ()] -> Maybe (Syn.Fields Surface.Pattern ()) -> Surface.Pattern ()
constructorPattern name params fields =
  wrapSimple
    ( Pat.PatternSimpleConsF
        Syn.ConstructorApp
          { name = anyType C.SimpleTypeIdentifier{name, ann = ()}
          , params = fmap ((,()) . Vector.fromList) params
          , fields = fmap (,()) fields
          , ann = ()
          }
        ()
    )

fieldsTuple :: [Surface.Pattern ()] -> Syn.Fields Surface.Pattern ()
fieldsTuple ps = Syn.FieldsTuple (Vector.fromList ps) ()

fieldsNamed :: [(Text, Surface.Pattern ())] -> Syn.Fields Surface.Pattern ()
fieldsNamed ps =
  let mapped =
        fmap
          (Bifunctor.first mkVar)
          ps
   in Syn.FieldsNamed (Vector.fromList mapped) ()

requireList :: String -> [a] -> ListNE.NonEmpty a
requireList label [] = error (label <> ": expected non-empty list")
requireList _label (x : xs) = x ListNE.:| xs

requireVector :: String -> [a] -> NE.NonEmptyVector a
requireVector label = NE.fromNonEmpty . requireList label

spec :: Spec
spec = describe "Pattern parser (minimal subset)" do
  it "parses wildcard _" do
    testParser "_" PPat.pPattern (Just wildcardPattern)

  it "parses literal true" do
    testParser "true" PPat.pPattern (Just (literalBoolPattern True))

  it "parses variable x" do
    testParser "x" PPat.pPattern (Just (varPattern "x"))

  it "parses tuple (x, y)" do
    testParser "(x, y)" PPat.pPattern (Just (tuplePattern [varPattern "x", varPattern "y"]))

  it "parses constructor Some(1)" do
    let expected =
          constructorPattern
            "Some"
            Nothing
            (Just (fieldsTuple [literalIntPattern 1]))
    testParser "Some(1)" PPat.pPattern (Just expected)

  it "parses constructor with named fields Cons { head = 1, tail = xs }" do
    let expected =
          constructorPattern
            "Cons"
            Nothing
            ( Just
                ( fieldsNamed
                    [ ("head", literalIntPattern 1)
                    , ("tail", varPattern "xs")
                    ]
                )
            )
    testParser "Cons { head = 1, tail = xs }" PPat.pPattern (Just expected)
