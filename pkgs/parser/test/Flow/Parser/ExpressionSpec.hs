module Flow.Parser.ExpressionSpec (spec) where

import "base" Data.Functor ((<&>))
import "base" Data.Maybe (fromJust)
import "hspec" Test.Hspec (Spec, describe, it)
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector
import "text" Data.Text (Text)
import "vector" Data.Vector qualified as Vector

import Flow.AST.Surface qualified as Surface
import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Constraint qualified as Surface
import Flow.AST.Surface.Expr qualified as Surface
import Flow.AST.Surface.Literal qualified as Surface
import Flow.AST.Surface.Pattern qualified as Surface
import Flow.AST.Surface.Syntax qualified as Surface
import Flow.AST.Surface.Type qualified as Surface.Type
import Flow.Parser (pExpression)
import Flow.Parser.SpecHelpers (shouldBe, shouldBeParsed, testParser)

literalInt :: Integer -> Surface.Expression ()
literalInt n = Surface.Expression{expr = Surface.ELiteral (Surface.LitInteger n), ann = ()}

var :: Text -> Surface.Expression ()
var name =
  Surface.Expression
    { expr =
        Surface.EVar
          Surface.AnyVarIdentifier
            { qualifier = mempty
            , typeQualifier = Nothing
            , identifier = Surface.SimpleVarIdentifier{name, ann = ()}
            , ann = ()
            }
    , ann = ()
    }

parens :: Surface.Expression () -> Surface.Expression ()
parens inner = Surface.Expression{expr = Surface.EParens inner, ann = ()}

unOp :: Surface.UnOp () -> Surface.Expression () -> Surface.Expression ()
unOp op expr =
  Surface.Expression
    { expr = Surface.EUnOpF Surface.UnOpExpression{op, operand = expr}
    , ann = ()
    }

binOp :: Surface.BinOp () -> Surface.Expression () -> Surface.Expression () -> Surface.Expression ()
binOp op l r =
  Surface.Expression
    { expr = Surface.EBinOpF Surface.BinOpExpression{op, left = l, right = r, ann = ()}
    , ann = ()
    }

tupleExpr :: [Surface.Expression ()] -> Surface.Expression ()
tupleExpr = \case
  expr1 : expr2 : rest ->
    Surface.Expression
      { expr =
          Surface.ETupleF
            expr1
            (NonEmptyVector.consV expr2 (Vector.fromList rest))
      , ann = ()
      }
  _ -> error "tuple should have at least two elements"

callUnnamed :: Text -> [Surface.Expression ()] -> Surface.Expression ()
callUnnamed fname args =
  Surface.Expression
    { expr =
        Surface.EAppF
          Surface.AppF
            { callee = var fname
            , typeParams = Nothing
            , args = Surface.AppArgsUnnamedF (Vector.fromList args)
            , with = Nothing
            , ann = ()
            }
    , ann = ()
    }

callNamed :: Text -> [(Text, Surface.Expression ())] -> Surface.Expression ()
callNamed fname args =
  Surface.Expression
    { expr =
        Surface.EAppF
          Surface.AppF
            { callee = var fname
            , typeParams = Nothing
            , args =
                Surface.AppArgsNamedF
                  ( Vector.fromList
                      ( args <&> \(name, expr) ->
                          Surface.ArgNamedF
                            { name = Surface.SimpleVarIdentifier name ()
                            , value = Just expr
                            , ann = ()
                            }
                      )
                  )
            , with = Nothing
            , ann = ()
            }
    , ann = ()
    }

callWithParams :: Text -> [Surface.Type ()] -> [Surface.Expression ()] -> Surface.Expression ()
callWithParams fname types args =
  Surface.Expression
    { expr =
        Surface.EAppF
          Surface.AppF
            { callee = var fname
            , typeParams =
                if not (null types)
                  then
                    Just
                      Surface.BindersAppF
                        { types = fromJust $ NonEmptyVector.fromList types
                        , ann = ()
                        }
                  else Nothing
            , args = Surface.AppArgsUnnamedF (Vector.fromList args)
            , with = Nothing
            , ann = ()
            }
    , ann = ()
    }

regionIdent :: Text -> Surface.Type ()
regionIdent name = Surface.Type{ty = Surface.Type.TyRegionF (Surface.RegionIdentifier{name, ann = ()}), ann = ()}

typeVar :: Text -> Surface.Type ()
typeVar name =
  Surface.Type
    { ty =
        Surface.Type.TyIdentifierF
          Surface.AnyTypeIdentifier
            { qualifier = Nothing
            , typeQualifier = Nothing
            , identifier = Surface.SimpleTypeIdentifier{name, ann = ()}
            , ann = ()
            }
    , ann = ()
    }

dotExpr :: Surface.Expression () -> Text -> Surface.Expression ()
dotExpr base field =
  Surface.Expression
    { expr =
        Surface.EDotAccess
          base
          Surface.AnyVarIdentifier
            { qualifier = mempty
            , typeQualifier = Nothing
            , identifier = Surface.SimpleVarIdentifier{name = field, ann = ()}
            , ann = ()
            }
    , ann = ()
    }

indexExpr :: Surface.Expression () -> Surface.Expression () -> Surface.Expression ()
indexExpr arr idx = Surface.Expression{expr = Surface.EIndex arr idx, ann = ()}

patternSimpleVar :: Bool -> Text -> Surface.PatternSimple ()
patternSimpleVar mut name =
  Surface.PatternSimple
    { patternSimple =
        Surface.PatSimVarF
          ( Surface.PatternVariableF
              { ref = Nothing
              , mut = if mut then Just () else Nothing
              , name = Surface.SimpleVarIdentifier{name, ann = ()}
              , ann = ()
              }
          )
    , ann = ()
    }

letStatement :: Text -> Surface.Expression () -> Surface.Statement ()
letStatement name rhsExpr =
  Surface.Statement
    { stmt =
        Surface.SLetF
          Surface.LetDefinitionF
            { lhs = patternSimpleVar False name
            , lhsType = Nothing
            , rhs = rhsExpr
            , ann = ()
            }
    , ann = ()
    }

blockExpr :: [Surface.Statement ()] -> Maybe (Surface.Expression ()) -> Surface.Expression ()
blockExpr stmts resultExpr =
  Surface.Expression
    { expr =
        Surface.EBlockF $
          Surface.CodeBlockF
            { region = Nothing
            , uses = mempty
            , statements = Vector.fromList stmts
            , result = resultExpr
            , ann = ()
            }
    , ann = ()
    }

spec :: Spec
spec = describe "Expression parser (minimal subset)" do
  it "parses literal 1" do
    testParser "1" pExpression $ shouldBeParsed (`shouldBe` literalInt 1)

  it "parses variable x" do
    testParser "x" pExpression $ shouldBeParsed (`shouldBe` var "x")

  it "parses parens (x)" do
    testParser "(x)" pExpression $ shouldBeParsed (`shouldBe` parens (var "x"))

  it "parses unary &x, &mut x, &'s x, -x, !x" do
    let cases =
          [ ("&x", Surface.UnOpTakeRef Nothing ())
          , ("&mut x", Surface.UnOpTakeMutRef Nothing ())
          , ("&'s x", Surface.UnOpTakeRef (Just Surface.RegionIdentifier{name = "s", ann = ()}) ())
          , ("-x", Surface.UnOpNeg ())
          , ("!x", Surface.UnOpNot ())
          ]
    mapM_
      (\(txt, op) -> testParser txt pExpression $ shouldBeParsed (`shouldBe` unOp op (var "x")))
      cases

  it "parses binary 1 + 2 * 3" do
    let expected =
          binOp
            (Surface.BinOpAdd ())
            (literalInt 1)
            (binOp (Surface.BinOpMul ()) (literalInt 2) (literalInt 3))
    testParser "1 + 2 * 3" pExpression $ shouldBeParsed (`shouldBe` expected)

  it "parses calls f(a, b) and with named args f { x = 1, y = 2 }" do
    testParser "f(a, b)" pExpression $ shouldBeParsed (`shouldBe` callUnnamed "f" [var "a", var "b"])
    testParser "f { x = 1, y = 2 }" pExpression $ shouldBeParsed (`shouldBe` callNamed "f" [("x", literalInt 1), ("y", literalInt 2)])

  it "parses calls with \"with\" clauses" do
    let source =
          """
          f(a, b) with {
            Reader<R> = reader_handle(env),
            state = state_handle(initial),
            writer = w,
          }
          """
    testParser source pExpression $ shouldBeParsed $ const $ pure ()

  it "parses calls with \"with\" clauses and in blocks" do
    let source =
          """
          my_func(a, b) with {
            writer = writer_handle() in {
              State<W> = s,
              Reader<R> = r,
            },
          }
          """
    testParser source pExpression $ shouldBeParsed $ const $ pure ()

  it "parses match expression" do
    let source =
          """
          match x {
            (v, a) => v,
            Some(v) => v,
            None => 0,
          }
          """
    testParser source pExpression $ shouldBeParsed $ const $ pure ()

  it "parses with block" do
    let source =
          """
          with {
            let Reader<R> = reader_handle(env);
            let state: State<S> = state_handle(initial);

            Reader<R> = reader_handle(env);
            state = state_handle(initial);
            Reader<R> = r;
            state = State<S>;

            let reader = Reader<R>;
            let state1 = state;

            let Reader<R>, State<S> = reader_state_handle(env, initial);
            let reader, state2 = reader_state_handle(env, initial);
            let reader1: Reader<R>, state2: State<S> = reader_state_handle(env, initial);
            reader1, state2 = reader_state_handle(env, initial);
          } in {
            ()
          }
          """
    testParser source pExpression $ shouldBeParsed $ const $ pure ()

  it "parses call with type/region params f<'s, T>(a)" do
    let expected = callWithParams "f" [regionIdent "s", typeVar "T"] [var "a"]
    testParser "f<'s, T>(a)" pExpression $ shouldBeParsed (`shouldBe` expected)

  it "parses chained access a.b[0]" do
    let expected = indexExpr (dotExpr (var "a") "b") (literalInt 0)
    testParser "a.b[0]" pExpression $ shouldBeParsed (`shouldBe` expected)

  it "parses tuple (a, b)" do
    testParser "(a, b)" pExpression $ shouldBeParsed (`shouldBe` tupleExpr [var "a", var "b"])

  it "parses full lambda" do
    let source =
          """
          <T>|a: T, b: T| -> @['r, Reader<T>, ..R] T
            where Monoid<T>
          {
            let r = Reader::ask();
            a ++ b ++ r
          }
          """
    testParser source pExpression $ shouldBeParsed $ const $ pure ()

  it "parses simple block { let x = 1; x }" do
    let stmt = letStatement "x" (literalInt 1)
        expected = blockExpr [stmt] (Just (var "x"))
    testParser "{ let x = 1; x }" pExpression $ shouldBeParsed (`shouldBe` expected)

  it "parses sequence of dot accesses with function calls with lambdas" do
    let source =
          """
          x
            .map(|y| y + 1)
            .filter(|y| y > 0)
            .sum()
            .for_each(|y| println(y)) with { Printer = printer_handle() }
          """
    testParser source pExpression $ shouldBeParsed (const $ pure ())
