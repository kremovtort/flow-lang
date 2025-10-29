module Flow.Parser.Operators where

import "base" Data.Functor ((<&>))
import "parser-combinators" Control.Monad.Combinators.Expr qualified as Expr

import Flow.AST.Surface (Expression (..))
import Flow.AST.Surface.Common
import Flow.AST.Surface.Expr
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (Parser, SourceRegion (..), scopeIdentifier, single)
import Text.Megaparsec qualified as Megaparsec

pOperators ::
  Parser (Expression SourceRegion) ->
  Parser (Expression SourceRegion)
pOperators pExpr = Expr.makeExprParser pExpr operators

operators ::
  [[Expr.Operator Parser (Expression SourceRegion)]]
operators =
  [ [deref, not', neg, takeRef, takeMutRef]
  , [mul, divOp, modOp]
  , [add, sub]
  , [concatOp]
  , [shiftLeft, shiftRight]
  , [bitwiseAnd, bitwiseOr]
  , [equal, notEqual, lessThan, lessThanOrEqual, greaterThan, greaterThanOrEqual]
  , [boolAnd]
  , [boolOr]
  ]
 where
  deref =
    Expr.Prefix $
      single (Lexer.Punctuation Lexer.Star)
        <&> \tok -> mkUnOp UnOpDeref tok.region

  not' =
    Expr.Prefix $
      single (Lexer.Punctuation Lexer.Not)
        <&> \tok -> mkUnOp UnOpNot tok.region

  neg =
    Expr.Prefix $
      single (Lexer.Punctuation Lexer.Minus) <&> \tok -> mkUnOp UnOpNeg tok.region

  takeRef = Expr.Prefix $ Megaparsec.try do
    tok <- single (Lexer.Punctuation Lexer.Ampersand)
    mScope <- Megaparsec.optional scopeIdentifier
    pure case mScope of
      Nothing -> mkUnOp (UnOpTakeRef Nothing) tok.region
      Just scope ->
        mkUnOp
          (UnOpTakeRef (Just scope))
          (SourceRegion tok.region.start scope.ann.end)

  takeMutRef = Expr.Prefix $ Megaparsec.try do
    tok <- single (Lexer.Punctuation Lexer.Ampersand)
    mScope <- Megaparsec.optional scopeIdentifier
    mutTok <- single (Lexer.Keyword Lexer.Mut)
    let region = SourceRegion tok.region.start mutTok.region.end
    pure $ mkUnOp (UnOpTakeMutRef mScope) region

  mul = binaryL Lexer.Star BinOpMul
  divOp = binaryL Lexer.Slash BinOpDiv
  modOp = binaryL Lexer.Percent BinOpMod

  add = binaryL Lexer.Plus BinOpAdd
  sub = binaryL Lexer.Minus BinOpSub

  concatOp = binaryL Lexer.Concat BinOpConcat

  shiftLeft = binaryL Lexer.ShiftLeft BinOpBitwiseShiftLeft
  shiftRight = binaryL Lexer.ShiftRight BinOpBitwiseShiftRight

  bitwiseAnd = binaryL Lexer.Ampersand BinOpBitwiseAnd
  bitwiseOr = binaryL Lexer.Pipe BinOpBitwiseOr

  equal = binaryN Lexer.Equal BinOpEqual
  notEqual = binaryN Lexer.NotEqual BinOpNotEqual
  lessThan = binaryN Lexer.LessThan BinOpLessThan
  lessThanOrEqual = binaryN Lexer.LessThanOrEqual BinOpLessThanOrEqual
  greaterThan = binaryN Lexer.GreaterThan BinOpGreaterThan
  greaterThanOrEqual = binaryN Lexer.GreaterThanOrEqual BinOpGreaterThanOrEqual

  boolAnd = binaryR Lexer.And BinOpAnd
  boolOr = binaryR Lexer.Or BinOpOr

  binaryL token ctor =
    Expr.InfixL $
      single (Lexer.Punctuation token) <&> \tok lhs rhs ->
        mkBinOp ctor tok.region lhs rhs

  binaryR token ctor =
    Expr.InfixR $
      single (Lexer.Punctuation token) <&> \tok lhs rhs ->
        mkBinOp ctor tok.region lhs rhs

  binaryN token ctor =
    Expr.InfixN $
      single (Lexer.Punctuation token) <&> \tok lhs rhs ->
        mkBinOp ctor tok.region lhs rhs

  mkBinOp ctor opRegion left right =
    let region =
          SourceRegion
            { start = left.ann.start
            , end = right.ann.end
            }
     in Expression
          { expr =
              EBinOp
                BinOpExpression
                  { op = ctor opRegion
                  , left = left
                  , right = right
                  , ann = region
                  }
          , ann = region
          }

  mkUnOp mkOp opAnn operand =
    Expression
      { expr = EUnOp UnOpExpression{op = mkOp opAnn, operand}
      , ann = SourceRegion opAnn.start operand.ann.end
      }
