module Flow.Parser.Syntax where

import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector
import "vector" Data.Vector qualified as Vector

import Data.Set qualified as Set
import Flow.AST.Surface (Expression (..), LHSExpression (..), Pattern (..), Type (..))
import Flow.AST.Surface.Common (SimpleVarIdentifier (..))
import Flow.AST.Surface.Syntax
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common

codeBlock ::
  Parser (LHSExpression Lexer.SourceRegion) ->
  Parser (Pattern Lexer.SourceRegion) ->
  Parser (Type Lexer.SourceRegion) ->
  Parser (Expression Lexer.SourceRegion) ->
  Parser (CodeBlockF LHSExpression Pattern Type Expression Lexer.SourceRegion)
codeBlock lhsExpr pat ty expr = do
  tokS <- single (Lexer.Punctuation Lexer.LeftBrace)
  statements <- Megaparsec.many (statement lhsExpr pat ty expr)
  result <- Megaparsec.optional expr
  tokE <- single (Lexer.Punctuation Lexer.RightBrace)
  let ann = Lexer.SourceRegion{start = tokS.payload.start, end = tokE.payload.end}
  pure $ CodeBlock{statements = Vector.fromList statements, result = result, ann = ann}

statement ::
  Parser (LHSExpression Lexer.SourceRegion) ->
  Parser (Pattern Lexer.SourceRegion) ->
  Parser (Type Lexer.SourceRegion) ->
  Parser (Expression Lexer.SourceRegion) ->
  Parser (StatementF LHSExpression Pattern Type Expression Lexer.SourceRegion)
statement lhsExpr pat ty expr = do
  Megaparsec.choice
    [ letStatement
    , assignStatement
    , returnStatement
    , continueStatement
    , breakStatement
    , expressionStatement
    , matchStatement
    , ifStatement
    , loopStatement
    , whileStatement
    , forStatement
    ]
 where
  letStatement = do
    letTok <- single (Lexer.Keyword Lexer.Let)
    mutTok <- Megaparsec.optional (single (Lexer.Keyword Lexer.Mut))
    lhs <- pat
    lhsType <- Megaparsec.optional do
      colonTok <- single (Lexer.Punctuation Lexer.Colon)
      ty' <- ty
      pure (ty', colonTok.payload)
    _ <- single (Lexer.Punctuation Lexer.Assign)
    rhs <- expr
    semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
    let ann = Lexer.SourceRegion{start = letTok.payload.start, end = semicolonTok.payload.end}
    pure $
      SLetF $
        LetDefinitionF
          { lhs = lhs
          , lhsAnn = lhs.ann
          , lhsType = lhsType
          , mutability = (.payload) <$> mutTok
          , rhs = rhs
          , rhsAnn = rhs.ann
          , ann = ann
          }

  assignStatement = do
    lhs <- lhsExpr
    _ <- single (Lexer.Punctuation Lexer.Assign)
    rhs <- expr
    semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
    let ann = Lexer.SourceRegion{start = lhs.ann.start, end = semicolonTok.payload.end}
    pure $
      SAssignF $
        AssignStatementF
          { lhs = lhs
          , lhsAnn = lhs.ann
          , rhs = rhs
          , rhsAnn = rhs.ann
          , ann = ann
          }

  returnStatement = do
    returnTok <- single (Lexer.Keyword Lexer.Return)
    expr' <- expr
    semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
    let ann = Lexer.SourceRegion{start = returnTok.payload.start, end = semicolonTok.payload.end}
    pure $ SReturnF expr' ann

  continueStatement = do
    continueTok <- single (Lexer.Keyword Lexer.Continue)
    label <- Megaparsec.optional simpleVarIdentifier
    semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
    let ann = Lexer.SourceRegion{start = continueTok.payload.start, end = semicolonTok.payload.end}
    pure $ SContinueF label ann

  breakStatement = do
    breakTok <- single (Lexer.Keyword Lexer.Break)
    label <- Megaparsec.optional simpleVarIdentifier
    semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
    let ann = Lexer.SourceRegion{start = breakTok.payload.start, end = semicolonTok.payload.end}
    pure $ SBreakF label ann

  expressionStatement = do
    expr' <- expr
    semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
    let ann = Lexer.SourceRegion{start = expr'.ann.start, end = semicolonTok.payload.end}
    pure $ SExpressionF expr' ann

  matchStatement = do
    expr' <- matchExpression pat expr
    pure $ SMatchF expr' expr'.ann

  ifStatement = do
    expr' <- ifExpression expr
    pure $ SIfF expr' expr'.ann

  loopStatement = do
    expr' <- loopExpression expr
    pure $ SLoopF expr' expr'.ann

  whileStatement = do
    expr' <- whileExpression expr
    pure $ SWhileF expr' expr'.ann

  forStatement = do
    expr' <- forExpression pat expr
    pure $ SForF expr' expr'.ann

matchExpression ::
  Parser (Pattern Lexer.SourceRegion) ->
  Parser (Expression Lexer.SourceRegion) ->
  Parser (MatchExpression Pattern Expression Lexer.SourceRegion)
matchExpression pat expr = do
  matchTok <- single (Lexer.Keyword Lexer.Match)
  value <- expr
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  arms <- Megaparsec.many do
    arm <- matchArm
    commaTok <- single (Lexer.Punctuation Lexer.Comma)
    pure (arm, Lexer.SourceRegion{start = arm.ann.start, end = commaTok.payload.end})
  lastArm <- do
    arm <- matchArm
    mCommaTok <- Megaparsec.optional (single (Lexer.Punctuation Lexer.Comma))
    let end = case mCommaTok of
          Just commaTok -> commaTok.payload.end
          Nothing -> arm.ann.end
    pure (arm, Lexer.SourceRegion{start = arm.ann.start, end = end})
  let armsAnn = case arms of
        [] -> snd lastArm
        (_, startAnn) : _ -> Lexer.SourceRegion{start = startAnn.start, end = (snd lastArm).end}
  rightBraceTok <- single (Lexer.Punctuation Lexer.RightBrace)
  let ann = Lexer.SourceRegion{start = matchTok.payload.start, end = rightBraceTok.payload.end}
  pure $
    MatchExpression
      { value = value
      , valueAnn = value.ann
      , arms = NonEmptyVector.snocV (Vector.fromList arms) lastArm
      , armsAnn = armsAnn
      , ann = ann
      }
 where
  matchArm = do
    pattern' <- pat
    guard <- Megaparsec.optional do
      guardTok <- single (Lexer.Keyword Lexer.If)
      guardExpr <- expr
      pure (guardExpr, guardTok.payload{Lexer.end = guardExpr.ann.end})
    _ <- single (Lexer.Punctuation Lexer.FatArrow)
    expr' <- expr
    pure $
      MatchArm
        { pattern = pattern'
        , patternAnn = pattern'.ann
        , guard = guard
        , expression = expr'
        , expressionAnn = expr'.ann
        , ann = pattern'.ann
        }

ifExpression ::
  Parser (Expression Lexer.SourceRegion) ->
  Parser (IfExpression Expression Lexer.SourceRegion)
ifExpression expr = do
  ifTok <- single (Lexer.Keyword Lexer.If)
  condition <- expr
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  then_ <- expr
  rightBraceTok <- single (Lexer.Punctuation Lexer.RightBrace)
  elseIfs <- Megaparsec.many elseIf
  else_ <- Megaparsec.optional do
    elseTok <- single (Lexer.Keyword Lexer.Else)
    _ <- single (Lexer.Punctuation Lexer.LeftBrace)
    else' <- expr
    _ <- single (Lexer.Punctuation Lexer.RightBrace)
    pure (else', Lexer.SourceRegion{start = elseTok.payload.start, end = rightBraceTok.payload.end})
  let ann =
        Lexer.SourceRegion
          { start = ifTok.payload.start
          , end = case else_ of
              Just (_, ann') -> ann'.end
              Nothing -> rightBraceTok.payload.end
          }
  pure $
    IfExpression
      { condition = condition
      , conditionAnn = condition.ann
      , then_ = then_
      , thenAnn = then_.ann
      , elseIfs = Vector.fromList elseIfs
      , else_ = else_
      , ann = ann
      }
 where
  elseIf = do
    _ <- single (Lexer.Keyword Lexer.Else)
    ifTok <- single (Lexer.Keyword Lexer.If)
    condition <- expr
    _ <- single (Lexer.Punctuation Lexer.LeftBrace)
    then_ <- expr
    rightBraceTok <- single (Lexer.Punctuation Lexer.RightBrace)
    pure
      ( condition
      , then_
      , Lexer.SourceRegion
          { start = ifTok.payload.start
          , end = rightBraceTok.payload.end
          }
      )

loopExpression ::
  Parser (Expression Lexer.SourceRegion) ->
  Parser (LoopExpression Expression Lexer.SourceRegion)
loopExpression expr = do
  label <- Megaparsec.optional do
    labelTok <- token
      (Set.singleton $ Megaparsec.Label "label")
      \case
        Lexer.RefScope s -> Just s
        _ -> Nothing
    _ <- single (Lexer.Punctuation Lexer.Colon)
    pure
      ( SimpleVarIdentifier
          { name = labelTok.token
          , ann = labelTok.payload
          }
      , Lexer.SourceRegion
          { start = labelTok.payload.start
          , end = labelTok.payload.end
          }
      )
  loopTok <- Megaparsec.try $ single (Lexer.Keyword Lexer.Loop)
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  body <- expr
  rightBraceTok <- single (Lexer.Punctuation Lexer.RightBrace)
  let ann =
        Lexer.SourceRegion
          { start = case label of
              Just (_, ann') -> ann'.start
              Nothing -> loopTok.payload.start
          , end = rightBraceTok.payload.end
          }
  pure $ LoopExpression{label = label, body = body, bodyAnn = body.ann, ann = ann}

whileExpression ::
  Parser (Expression Lexer.SourceRegion) ->
  Parser (WhileExpression Expression Lexer.SourceRegion)
whileExpression expr = do
  label <- Megaparsec.optional do
    labelTok <- token
      (Set.singleton $ Megaparsec.Label "label")
      \case
        Lexer.RefScope s -> Just s
        _ -> Nothing
    _ <- single (Lexer.Punctuation Lexer.Colon)
    pure
      ( SimpleVarIdentifier labelTok.token labelTok.payload
      , Lexer.SourceRegion
          { start = labelTok.payload.start
          , end = labelTok.payload.end
          }
      )
  whileTok <- Megaparsec.try $ single (Lexer.Keyword Lexer.While)
  condition <- expr
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  body <- expr
  rightBraceTok <- single (Lexer.Punctuation Lexer.RightBrace)
  let ann =
        Lexer.SourceRegion
          { start = case label of
              Just (_, ann') -> ann'.start
              Nothing -> whileTok.payload.start
          , end = rightBraceTok.payload.end
          }
  pure $ WhileExpression{label = label, condition = condition, conditionAnn = condition.ann, body = body, bodyAnn = body.ann, ann = ann}

forExpression ::
  Parser (Pattern Lexer.SourceRegion) ->
  Parser (Expression Lexer.SourceRegion) ->
  Parser (ForExpression Pattern Expression Lexer.SourceRegion)
forExpression pat expr = do
  label <- Megaparsec.optional do
    labelTok <- token
      (Set.singleton $ Megaparsec.Label "label")
      \case
        Lexer.RefScope s -> Just s
        _ -> Nothing
    _ <- single (Lexer.Punctuation Lexer.Colon)
    pure
      ( SimpleVarIdentifier labelTok.token labelTok.payload
      , Lexer.SourceRegion
          { start = labelTok.payload.start
          , end = labelTok.payload.end
          }
      )
  forTok <- Megaparsec.try $ single (Lexer.Keyword Lexer.For)
  pattern <- pat
  _ <- single (Lexer.Keyword Lexer.In)
  iterable <- expr
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  body <- expr
  rightBraceTok <- single (Lexer.Punctuation Lexer.RightBrace)
  let ann =
        Lexer.SourceRegion
          { start = case label of
              Just (_, ann') -> ann'.start
              Nothing -> forTok.payload.start
          , end = rightBraceTok.payload.end
          }
  pure $
    ForExpression
      { label = label
      , pattern = pattern
      , patternAnn = pattern.ann
      , iterable = iterable
      , iterableAnn = iterable.ann
      , body = body
      , bodyAnn = body.ann
      , ann = ann
      }
