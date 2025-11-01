module Flow.Parser.Callable where

import "megaparsec" Text.Megaparsec qualified as Megaparsec

import Data.Functor (void, (<&>))
import Data.Vector qualified as Vector
import Flow.AST.Surface.Callable (ArgF (..), CallableF (..), CallableHeader (..), FnDeclarationF, FnDefinitionF)
import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Constraint qualified as Surface
import Flow.AST.Surface.Syntax qualified as Surface
import Flow.Lexer (SourceRegion (..), WithPos (..))
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (HasAnn, Parser, pEffectsResult, simpleVarIdentifier, single)
import Flow.Parser.Constraint (pBindersWConstraints, pWhereBlockHead)
import Flow.Parser.Syntax (pCodeBlock)

pCallableHeader ::
  (HasAnn ty SourceRegion) =>
  Parser (WithPos ()) -> -- fn | op
  Parser (reciever SourceRegion) ->
  Parser (ty SourceRegion) ->
  Parser (CallableHeader ty reciever SourceRegion)
pCallableHeader pKind pReciever pTy = do
  kindTok <- pKind
  receiver <- pReciever
  name <- simpleVarIdentifier
  typeParams <- Megaparsec.optional (pBindersWConstraints pTy)
  _ <- single (Lexer.Punctuation Lexer.LeftParen)
  args <- Vector.fromList <$> pArgs
  argsEnd <- single (Lexer.Punctuation Lexer.RightParen)
  effectsResult <- Megaparsec.optional (pEffectsResult pTy)
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
                  Just (_, result) -> result.ann.end
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
  forall kind reciever ty body.
  (HasAnn ty SourceRegion) =>
  Parser (WithPos ()) -> -- fn | op
  Parser (reciever SourceRegion) ->
  Parser (ty SourceRegion) ->
  Parser (body SourceRegion, Maybe SourceRegion) ->
  Parser (CallableF kind reciever ty body SourceRegion)
pCallable pKind pReciever pTy pBody = do
  header <- pCallableHeader pKind pReciever pTy
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
pFnDeclaration pTy =
  pCallable
    (void <$> single (Lexer.Keyword Lexer.Fn))
    (pure Surface.UnitF)
    pTy
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
    pTy
    (pCodeBlock pStmt pExpr <&> \block -> (block, Just block.ann))
