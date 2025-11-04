module Flow.Parser.Callable where

import "megaparsec" Text.Megaparsec qualified as Megaparsec

import Data.Functor (void, (<&>))
import Data.Vector qualified as Vector
import Flow.AST.Surface.Callable (ArgF (..), CallableF (..), CallableHeader (..), FnDeclarationF, FnDefinitionF)
import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Constraint qualified as Surface
import Flow.AST.Surface.Syntax qualified as Surface
import Flow.AST.Surface.Type qualified as Surface
import Flow.Lexer (SourceRegion (..), WithPos (..))
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (HasAnn, Parser, simpleVarIdentifier, single)
import Flow.Parser.Constraint (pBindersWConstraints, pWhereBlockHead)
import Flow.Parser.Syntax (pCodeBlock)
import Flow.Parser.Type (pFnEffectsResult)

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
    (pure (Surface.UnitF, Nothing))

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
