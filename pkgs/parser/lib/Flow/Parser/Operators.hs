module Flow.Parser.Operators where

import "base" Data.Functor ((<&>))
import "parser-combinators" Control.Monad.Combinators.Expr qualified as Expr

import Flow.AST.Surface (Expression (..))
import Flow.AST.Surface.Common
import Flow.AST.Surface.Expr
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (Parser, SourceSpan (..), regionIdentifier, single)
import Text.Megaparsec qualified as Megaparsec

pOperators ::
  Parser (Expression SourceSpan) ->
  Parser (Expression SourceSpan)
pOperators pExpr = Expr.makeExprParser pExpr operators

operators ::
  [[Expr.Operator Parser (Expression SourceSpan)]]
operators =
  [ [deref, not', neg, takeMutRef, takeRef]
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
        <&> \tok -> mkUnOp UnOpDeref tok.span

  not' =
    Expr.Prefix $
      single (Lexer.Punctuation Lexer.Not)
        <&> \tok -> mkUnOp UnOpNot tok.span

  neg =
    Expr.Prefix $
      single (Lexer.Punctuation Lexer.Minus) <&> \tok -> mkUnOp UnOpNeg tok.span

  takeRef = Expr.Prefix $ Megaparsec.try do
    tok <- single (Lexer.Punctuation Lexer.Ampersand)
    mRegion <- Megaparsec.optional regionIdentifier
    pure case mRegion of
      Nothing -> mkUnOp (UnOpTakeRef Nothing) tok.span
      Just region ->
        mkUnOp
          (UnOpTakeRef (Just region))
          (SourceSpan tok.span.start region.ann.end)

  takeMutRef = Expr.Prefix $ Megaparsec.try do
    tok <- single (Lexer.Punctuation Lexer.Ampersand)
    mRegion <- Megaparsec.optional regionIdentifier
    mutTok <- single (Lexer.Keyword Lexer.Mut)
    let region = SourceSpan tok.span.start mutTok.span.end
    pure $ mkUnOp (UnOpTakeMutRef mRegion) region

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
        mkBinOp ctor tok.span lhs rhs

  binaryR token ctor =
    Expr.InfixR $
      single (Lexer.Punctuation token) <&> \tok lhs rhs ->
        mkBinOp ctor tok.span lhs rhs

  binaryN token ctor =
    Expr.InfixN $
      single (Lexer.Punctuation token) <&> \tok lhs rhs ->
        mkBinOp ctor tok.span lhs rhs

  mkBinOp ctor opRegion left right =
    let region =
          SourceSpan
            { start = left.ann.start
            , end = right.ann.end
            }
     in Expression
          { expr =
              EBinOpF
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
      { expr = EUnOpF UnOpExpression{op = mkOp opAnn, operand}
      , ann = SourceSpan opAnn.start operand.ann.end
      }
