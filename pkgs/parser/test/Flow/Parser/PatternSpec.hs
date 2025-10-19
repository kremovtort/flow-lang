module Flow.Parser.PatternSpec (spec) where

import "base" Data.Bifunctor qualified as Bifunctor
import "base" Data.Functor ((<&>))
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
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (Parser)
import Flow.Parser.Helpers (testParser)
import Flow.Parser.Pattern qualified as PPat

pPatternSimple :: Parser (Surface.PatternSimple Lexer.SourceRegion, Lexer.SourceRegion)
pPatternSimple = PPat.pPatternSimple pPatternSimple <&> \(simpleF, ann) ->
  (Surface.PatternSimple simpleF ann, ann)

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

wrapSimple ::
  Pat.PatternSimpleF Surface.PatternSimple Surface.Type () ->
  Surface.PatternSimple ()
wrapSimple simple =
  Surface.PatternSimple
    { patternSimple = simple
    , ann = ()
    }

wildcardPattern :: Surface.PatternSimple ()
wildcardPattern = wrapSimple Pat.PatternSimpleWildcardF

literalBoolPattern :: Bool -> Surface.PatternSimple ()
literalBoolPattern value =
  wrapSimple (Pat.PatternSimpleLiteralF (Lit.LitBool value))

literalIntPattern :: Integer -> Surface.PatternSimple ()
literalIntPattern value =
  wrapSimple (Pat.PatternSimpleLiteralF (Lit.LitInteger value))

varPattern :: Text -> Surface.PatternSimple ()
varPattern name = wrapSimple (Pat.PatternSimpleVarF (mkVar name))

tuplePattern :: [Surface.PatternSimple ()] -> Surface.PatternSimple ()
tuplePattern ps = wrapSimple (Pat.PatternSimpleTupleF (requireVector "tuplePattern" ps))

constructorPattern ::
  Text ->
  Maybe [C.SimpleTypeIdentifier ()] ->
  Maybe (Syn.Fields Surface.PatternSimple ()) ->
  Surface.PatternSimple ()
constructorPattern name params fields =
  wrapSimple
    ( Pat.PatternSimpleConsF
        Syn.ConstructorAppF
          { name = anyType C.SimpleTypeIdentifier{name, ann = ()}
          , params = fmap ((,()) . Vector.fromList) params
          , fields = fmap (,()) fields
          , ann = ()
          }
    )

fieldsTuple :: [Surface.PatternSimple ()] -> Syn.Fields Surface.PatternSimple ()
fieldsTuple ps = Syn.FieldsTuple (Vector.fromList (map (,()) ps))

fieldsNamed :: [(Text, Surface.PatternSimple ())] -> Syn.Fields Surface.PatternSimple ()
fieldsNamed ps =
  let mapped =
        map (\(a, b) -> (a, b, ())) $
          fmap
            (Bifunctor.first mkVar)
            ps
   in Syn.FieldsNamed (Vector.fromList mapped)

requireList :: String -> [a] -> ListNE.NonEmpty a
requireList label [] = error (label <> ": expected non-empty list")
requireList _label (x : xs) = x ListNE.:| xs

requireVector :: String -> [a] -> NE.NonEmptyVector a
requireVector label = NE.fromNonEmpty . requireList label

spec :: Spec
spec = describe "Pattern parser (minimal subset)" do
  it "parses wildcard _" do
    testParser "_" (fst <$> pPatternSimple) (Just wildcardPattern)

  it "parses literal true" do
    testParser "true" (fst <$> pPatternSimple) (Just (literalBoolPattern True))

  it "parses variable x" do
    testParser "x" (fst <$> pPatternSimple) (Just (varPattern "x"))

  it "parses tuple (x, y)" do
    testParser "(x, y)" (fst <$> pPatternSimple) (Just (tuplePattern [varPattern "x", varPattern "y"]))

  it "parses constructor Some(1)" do
    let expected =
          constructorPattern
            "Some"
            Nothing
            (Just (fieldsTuple [literalIntPattern 1]))
    testParser "Some(1)" (fst <$> pPatternSimple) (Just expected)

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
    testParser "Cons { head = 1, tail = xs }" (fst <$> pPatternSimple) (Just expected)
