module Flow.Parser.PatternSpec (spec) where

import "base" Data.Functor ((<&>))
import "base" Data.List.NonEmpty qualified as ListNE
import "base" GHC.Stack (HasCallStack)
import "hspec" Test.Hspec (Spec, describe, it)
import "nonempty-vector" Data.Vector.NonEmpty qualified as NE
import "text" Data.Text (Text)
import "vector" Data.Vector qualified as Vector

import Data.Maybe (fromJust)
import Flow.AST.Surface qualified as Surface
import Flow.AST.Surface.Common qualified as C
import Flow.AST.Surface.Constraint qualified as Surface
import Flow.AST.Surface.Literal qualified as Lit
import Flow.AST.Surface.Pattern qualified as Pat
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (Parser)
import Flow.Parser.Helpers (testParser)
import Flow.Parser.Pattern qualified as PPat

pPatternSimple :: Parser (Surface.PatternSimple Lexer.SourceRegion)
pPatternSimple =
  PPat.pPatternSimple pPatternSimple (fail "anyType") <&> uncurry Surface.PatternSimple

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
wildcardPattern = wrapSimple Pat.PatSimWildcardF

literalBoolPattern :: Bool -> Surface.PatternSimple ()
literalBoolPattern value =
  wrapSimple (Pat.PatSimLiteralF (Lit.LitBool value))

literalIntPattern :: Integer -> Surface.PatternSimple ()
literalIntPattern value =
  wrapSimple (Pat.PatSimLiteralF (Lit.LitInteger value))

varPattern :: Text -> Surface.PatternSimple ()
varPattern name =
  wrapSimple
    ( Pat.PatSimVarF
        ( Pat.PatternVariableF
            { mut = Nothing
            , name = mkVar name
            , ann = ()
            }
        )
    )

tuplePattern :: [Surface.PatternSimple ()] -> Surface.PatternSimple ()
tuplePattern ps = wrapSimple (Pat.PatSimTupleF (requireVector "tuplePattern" ps))

constructorPattern ::
  Text ->
  Maybe [C.SimpleTypeIdentifier ()] ->
  Maybe (Pat.PatternFieldsF Surface.PatternSimple Surface.Type ()) ->
  Surface.PatternSimple ()
constructorPattern name params fields =
  wrapSimple
    ( Pat.PatSimConstructorAppF
        Pat.PatternConsturctorAppF
          { name = anyType C.SimpleTypeIdentifier{name, ann = ()}
          , typeParams =
              params <&> \params' ->
                Surface.BindersF
                  { scopes = mempty
                  , types =
                      Vector.fromList $
                        params' <&> \param'' ->
                          Surface.BinderWoConstraintF
                            { name = param''
                            , typeType = Nothing
                            , ann = ()
                            }
                  , ann = ()
                  }
          , fields = (,()) <$> fields
          , ann = ()
          }
    )

fieldsTuple ::
  [Surface.PatternSimple ()] ->
  Pat.PatternFieldsF Surface.PatternSimple Surface.Type ()
fieldsTuple ps =
  Pat.PatFldsUnnamedF
    ( requireVector
        "fieldsTuple"
        ( ps <&> \p ->
            Pat.PatternFieldUnnamedF
              { value = p
              , optional = Nothing
              , ann = ()
              }
        )
    )

fieldsNamed ::
  [Pat.PatternFieldNamedF Surface.PatternSimple Surface.Type ()] ->
  Pat.PatternFieldsF Surface.PatternSimple Surface.Type ()
fieldsNamed ps = Pat.PatFldsNamedF (fromJust $ NE.fromList ps)

mkFieldNamed :: Text -> Surface.PatternSimple () -> Pat.PatternFieldNamedF Surface.PatternSimple Surface.Type ()
mkFieldNamed name value =
  Pat.PatFldNmdValueF $
    Pat.PatternFieldNamedValueF
      { name = mkVar name
      , optional = Nothing
      , value = value
      , ann = ()
      }

mkFieldNamedPun :: Text -> Pat.PatternFieldNamedF Surface.PatternSimple Surface.Type ()
mkFieldNamedPun name =
  Pat.PatFldNmdPunningF $
    Pat.PatternFieldNamedPunningF
      { mut = Nothing
      , name = mkVar name
      , optional = Nothing
      , ann = ()
      }

requireList :: (HasCallStack) => String -> [a] -> ListNE.NonEmpty a
requireList label [] = error (label <> ": expected non-empty list")
requireList _label (x : xs) = x ListNE.:| xs

requireVector :: (HasCallStack) => String -> [a] -> NE.NonEmptyVector a
requireVector label = NE.fromNonEmpty . requireList label

spec :: Spec
spec = describe "Pattern parser (minimal subset)" do
  it "parses wildcard _" do
    testParser "_" pPatternSimple (Just wildcardPattern)

  it "parses literal true" do
    testParser "true" pPatternSimple (Just (literalBoolPattern True))

  it "parses variable x" do
    testParser "x" pPatternSimple (Just (varPattern "x"))

  it "parses tuple (x, y)" do
    testParser "(x, y)" pPatternSimple (Just (tuplePattern [varPattern "x", varPattern "y"]))

  it "parses constructor Some(1)" do
    let expected =
          constructorPattern
            "Some"
            Nothing
            (Just (fieldsTuple [literalIntPattern 1]))
    testParser "Some(1)" pPatternSimple (Just expected)

  it "parses constructor with named fields Cons { head = 1, tail = xs }" do
    let expected =
          constructorPattern
            "Cons"
            Nothing
            ( Just
                ( fieldsNamed
                    [ mkFieldNamed "head" (literalIntPattern 1)
                    , mkFieldNamed "tail" (varPattern "xs")
                    ]
                )
            )
    testParser "Cons { head = 1, tail = xs }" pPatternSimple (Just expected)

  it "parses constructor with fields punning Cons { head, tail }" do
    let expected =
          constructorPattern
            "Cons"
            Nothing
            (Just (fieldsNamed [mkFieldNamedPun "head", mkFieldNamedPun "tail"]))
    testParser "Cons { head, tail }" pPatternSimple (Just expected)
