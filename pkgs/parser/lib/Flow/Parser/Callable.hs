{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Flow.Parser.Callable where

import "base" Data.Functor (void, (<&>))
import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "vector" Data.Vector qualified as Vector

import Flow.AST.Surface.Callable (
  ArgF (..),
  CallableF (..),
  CallableHeader (..),
  FnDeclarationF,
  FnDefinitionF,
  FnInfixDeclarationF,
  FnInfixDefinitionF,
  OpDeclarationF,
  OpDefinitionF,
  OpInfixDeclarationF,
  OpInfixDefinitionF,
  ReceiverHeaderF (..),
 )
import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Constraint qualified as Surface
import Flow.AST.Surface.Syntax qualified as Surface
import Flow.AST.Surface.Type qualified as Surface
import Flow.Lexer (SourceRegion (..), WithPos (..))
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (HasAnn, Parser, simpleVarIdentifier, single)
import Flow.Parser.Constraint (anyVarIdentifier, pBindersWConstraints, pWhereBlockHead)
import Flow.Parser.Syntax (pCodeBlock)
import Flow.Parser.Type (pFnEffectsResult)

pRecieverHeader ::
  (HasAnn ty SourceRegion) =>
  Parser (ty SourceRegion) ->
  Parser (ReceiverHeaderF ty SourceRegion)
pRecieverHeader pTy = do
  typeParams <- Megaparsec.optional (pBindersWConstraints pTy)
  name <- simpleVarIdentifier
  _ <- single (Lexer.Punctuation Lexer.Colon)
  type_ <- pTy
  pure
    ReceiverHeaderF
      { typeParams = typeParams
      , name = name
      , type_ = type_
      , ann =
          SourceRegion
            { start = case typeParams of
                Just typeParams' -> typeParams'.ann.start
                Nothing -> name.ann.start
            , end = type_.ann.end
            }
      }

pCallableHeader ::
  (HasAnn ty SourceRegion, HasAnn name SourceRegion) =>
  Parser (WithPos ()) -> -- fn | op
  Parser (reciever SourceRegion) ->
  Parser (name SourceRegion) ->
  Parser (ty SourceRegion) ->
  Parser (CallableHeader reciever name ty SourceRegion)
pCallableHeader pKind pReciever pName pTy = do
  kindTok <- pKind
  receiver <- pReciever
  name <- pName
  typeParams <- Megaparsec.optional (pBindersWConstraints pTy)
  _ <- single (Lexer.Punctuation Lexer.LeftParen)
  args <- Vector.fromList <$> pArgs
  argsEnd <- single (Lexer.Punctuation Lexer.RightParen)
  effectsResult <- Megaparsec.optional (pFnEffectsResult pTy)
  whereBlock <- Megaparsec.optional (pWhereBlockHead pTy)
  pure
    CallableHeader
      { receiver = receiver
      , name = name
      , typeParams = typeParams
      , args = args
      , effectsResult = effectsResult
      , whereBlock = whereBlock
      , ann =
          SourceRegion
            { start = kindTok.region.start
            , end = case whereBlock of
                Just whereBlock' -> whereBlock'.ann.end
                Nothing -> case effectsResult of
                  Just effectsResult' -> effectsResult'.ann.end
                  Nothing -> argsEnd.region.end
            }
      }
 where
  pArgs = Megaparsec.sepEndBy1 pArg (single (Lexer.Punctuation Lexer.Comma))

  pArg = do
    mut <- Megaparsec.optional (single (Lexer.Keyword Lexer.Mut))
    name <- simpleVarIdentifier
    _ <- single (Lexer.Punctuation Lexer.Colon)
    type_ <- pTy
    pure $
      ArgF
        { mut = fmap (.region) mut
        , name = name
        , type_ = type_
        , ann =
            SourceRegion
              { start = case mut of
                  Just mut' -> mut'.region.start
                  Nothing -> name.ann.start
              , end = type_.ann.end
              }
        }

pCallable ::
  forall kind reciever name body ty.
  (HasAnn ty SourceRegion, HasAnn name SourceRegion) =>
  Parser (WithPos ()) -> -- fn | op
  Parser (reciever SourceRegion) ->
  Parser (name SourceRegion) ->
  Parser (body SourceRegion, Maybe SourceRegion) ->
  Parser (ty SourceRegion) ->
  Parser (CallableF kind reciever name body ty SourceRegion)
pCallable pKind pReciever pName pBody pTy = do
  header <- pCallableHeader pKind pReciever pName pTy
  (body, bodyAnn) <- pBody
  pure
    CallableF
      { header
      , body
      , ann =
          SourceRegion
            { start = header.ann.start
            , end = case bodyAnn of
                Just bodyAnn' -> bodyAnn'.end
                Nothing -> header.ann.end
            }
      }

pFnDeclaration ::
  (HasAnn ty SourceRegion) =>
  Parser (ty SourceRegion) ->
  Parser (FnDeclarationF ty SourceRegion)
pFnDeclaration =
  pCallable
    (void <$> single (Lexer.Keyword Lexer.Fn))
    (pure Surface.UnitF)
    simpleVarIdentifier
    pBodySemicolon

pFnInfixDeclaration ::
  (HasAnn ty SourceRegion) =>
  Parser (ty SourceRegion) ->
  Parser (FnInfixDeclarationF ty SourceRegion)
pFnInfixDeclaration pTy =
  pCallable
    (void <$> single (Lexer.Keyword Lexer.Fn))
    (pRecieverHeader pTy)
    simpleVarIdentifier
    pBodySemicolon
    pTy

pFnDefinition ::
  (HasAnn ty SourceRegion, HasAnn expr SourceRegion, HasAnn stmt SourceRegion) =>
  Parser (stmt SourceRegion) ->
  Parser (ty SourceRegion) ->
  Parser (expr SourceRegion) ->
  Parser (FnDefinitionF stmt ty expr SourceRegion)
pFnDefinition pStmt pTy pExpr =
  pCallable
    (void <$> single (Lexer.Keyword Lexer.Fn))
    (pure Surface.UnitF)
    simpleVarIdentifier
    (pCodeBlock pStmt pExpr <&> \block -> (block, Just block.ann))
    pTy

pFnInfixDefinition ::
  (HasAnn ty SourceRegion, HasAnn expr SourceRegion, HasAnn stmt SourceRegion) =>
  Parser (stmt SourceRegion) ->
  Parser (ty SourceRegion) ->
  Parser (expr SourceRegion) ->
  Parser (FnInfixDefinitionF stmt ty expr SourceRegion)
pFnInfixDefinition pStmt pTy pExpr =
  pCallable
    (void <$> single (Lexer.Keyword Lexer.Fn))
    (pRecieverHeader pTy)
    simpleVarIdentifier
    (pCodeBlock pStmt pExpr <&> \block -> (block, Just block.ann))
    pTy

pOpDeclaration ::
  (HasAnn ty SourceRegion) =>
  Parser (ty SourceRegion) ->
  Parser (OpDeclarationF ty SourceRegion)
pOpDeclaration =
  pCallable
    (void <$> single (Lexer.Keyword Lexer.Op))
    (pure Surface.UnitF)
    simpleVarIdentifier
    pBodySemicolon

pOpInfixDeclaration ::
  (HasAnn ty SourceRegion) =>
  Parser (ty SourceRegion) ->
  Parser (OpInfixDeclarationF ty SourceRegion)
pOpInfixDeclaration pTy =
  pCallable
    (void <$> single (Lexer.Keyword Lexer.Op))
    (pRecieverHeader pTy)
    simpleVarIdentifier
    pBodySemicolon
    pTy

pOpDefinition ::
  (HasAnn ty SourceRegion, HasAnn expr SourceRegion, HasAnn stmt SourceRegion) =>
  Parser (stmt SourceRegion) ->
  Parser (ty SourceRegion) ->
  Parser (expr SourceRegion) ->
  Parser (OpDefinitionF stmt ty expr SourceRegion)
pOpDefinition pStmt pTy pExpr =
  pCallable
    (void <$> single (Lexer.Keyword Lexer.Op))
    (pure Surface.UnitF)
    (anyVarIdentifier pTy)
    (pCodeBlock pStmt pExpr <&> \block -> (block, Just block.ann))
    pTy

pOpInfixDefinition ::
  (HasAnn ty SourceRegion, HasAnn expr SourceRegion, HasAnn stmt SourceRegion) =>
  Parser (stmt SourceRegion) ->
  Parser (ty SourceRegion) ->
  Parser (expr SourceRegion) ->
  Parser (OpInfixDefinitionF stmt ty expr SourceRegion)
pOpInfixDefinition pStmt pTy pExpr =
  pCallable
    (void <$> single (Lexer.Keyword Lexer.Op))
    (pRecieverHeader pTy)
    (anyVarIdentifier pTy)
    (pCodeBlock pStmt pExpr <&> \block -> (block, Just block.ann))
    pTy

pBodySemicolon :: Parser (Surface.UnitF ann, Maybe SourceRegion)
pBodySemicolon = single (Lexer.Punctuation Lexer.Semicolon) <&> \semicolon ->
  (Surface.UnitF, Just semicolon.region)
