{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Flow.Parser.Module where

import "base" Data.Functor ((<&>))
import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "vector" Data.Vector qualified as Vector

import Flow.AST.Surface.Callable qualified as Surface
import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Constraint qualified as Surface
import Flow.AST.Surface.Decl qualified as Surface
import Flow.AST.Surface.Module qualified as Surface
import Flow.AST.Surface.Syntax qualified as Surface
import Flow.AST.Surface.Use qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Callable (pFnDefinition, pFnInfixDefinition)
import Flow.Parser.Common (HasAnn, Parser, moduleIdentifier, pPub, single)
import Flow.Parser.Constraint (pTypeDefinition)
import Flow.Parser.Decl (pEffect, pEnum, pStruct, pTrait)
import Flow.Parser.Syntax (pLetDefinition)
import Flow.Parser.Use (pUseClause)

pModDefinitionBody ::
  ( HasAnn mod Lexer.SourceRegion
  , HasAnn stmt Lexer.SourceRegion
  , HasAnn simPat Lexer.SourceRegion
  , HasAnn ty Lexer.SourceRegion
  , HasAnn expr Lexer.SourceRegion
  ) =>
  Parser (mod Lexer.SourceRegion) ->
  Parser (stmt Lexer.SourceRegion) ->
  Parser (simPat Lexer.SourceRegion) ->
  Parser (ty Lexer.SourceRegion) ->
  Parser (expr Lexer.SourceRegion) ->
  Parser (Surface.ModDefinitionBodyF mod stmt simPat ty expr Lexer.SourceRegion)
pModDefinitionBody pMod' pStmt pSimPat pTy pExpr = do
  uses <- Megaparsec.many pUseClause
  items <- Megaparsec.many (pModuleItem pMod' pStmt pSimPat pTy pExpr)
  pure $
    Surface.ModDefinitionBodyF
      { uses = Vector.fromList uses
      , items = Vector.fromList items
      }

pModuleItem ::
  ( HasAnn mod Lexer.SourceRegion
  , HasAnn stmt Lexer.SourceRegion
  , HasAnn simPat Lexer.SourceRegion
  , HasAnn ty Lexer.SourceRegion
  , HasAnn expr Lexer.SourceRegion
  ) =>
  Parser (mod Lexer.SourceRegion) ->
  Parser (stmt Lexer.SourceRegion) ->
  Parser (simPat Lexer.SourceRegion) ->
  Parser (ty Lexer.SourceRegion) ->
  Parser (expr Lexer.SourceRegion) ->
  Parser (Surface.ModuleItemF mod stmt simPat ty expr Lexer.SourceRegion)
pModuleItem pMod' pStmt pSimPat pTy pExpr = do
  pub <- Megaparsec.optional pPub
  item <- pModuleItemVariant
  let ann =
        Lexer.SourceRegion
          { start = case pub of
              Just (_, region) -> region.start
              Nothing -> (snd item).start
          , end = (snd item).end
          }
  pure
    Surface.ModuleItemF
      { pub = fmap fst pub
      , item = fst item
      , ann
      }
 where
  pModuleItemVariant = do
    Megaparsec.choice
      [ withRegion Surface.ModItemModF <$> pMod'
      , withRegion Surface.ModItemStructF <$> pStruct pTy
      , withRegion Surface.ModItemEnumF <$> pEnum pTy
      , withRegion Surface.ModItemTypeAliasF
          <$> (pTypeDefinition pTy <* single (Lexer.Punctuation Lexer.Semicolon))
      , withRegion Surface.ModItemLetF <$> pLetDefinition pSimPat pTy pExpr
      , withRegion Surface.ModItemTraitF <$> pTrait pStmt pTy pExpr
      , withRegion Surface.ModItemEffectF <$> pEffect pStmt pTy pExpr
      , pPubUse <&> \(pub, use, ann) ->
          (Surface.ModItemPubUseF pub use, ann)
      , withRegion Surface.ModItemFnF <$> Megaparsec.try (pFnDefinition pStmt pTy pExpr)
      , withRegion Surface.ModItemFnInfixF <$> Megaparsec.try (pFnInfixDefinition pStmt pTy pExpr)
      ]

  withRegion f item = (f item, item.ann)

pMod ::
  ( HasAnn mod Lexer.SourceRegion
  , HasAnn stmt Lexer.SourceRegion
  , HasAnn simPat Lexer.SourceRegion
  , HasAnn ty Lexer.SourceRegion
  , HasAnn expr Lexer.SourceRegion
  ) =>
  Parser (mod Lexer.SourceRegion) ->
  Parser (stmt Lexer.SourceRegion) ->
  Parser (simPat Lexer.SourceRegion) ->
  Parser (ty Lexer.SourceRegion) ->
  Parser (expr Lexer.SourceRegion) ->
  Parser (Surface.ModF mod stmt simPat ty expr Lexer.SourceRegion, Lexer.SourceRegion)
pMod pMod' pStmt pSimPat pTy pExpr =
  Megaparsec.choice
    [ Megaparsec.try pModDeclaration
    , pModDefinition pMod' pStmt pSimPat pTy pExpr
    ]

pModDeclaration ::
  Parser (Surface.ModF mod lhsExpr simPat ty expr Lexer.SourceRegion, Lexer.SourceRegion)
pModDeclaration = do
  modTok <- single (Lexer.Keyword Lexer.Mod)
  ident <- moduleIdentifier
  tokE <- single (Lexer.Punctuation Lexer.Semicolon)
  pure (Surface.ModDeclarationF ident, Lexer.SourceRegion{start = modTok.region.start, end = tokE.region.end})

pModDefinition ::
  ( HasAnn mod Lexer.SourceRegion
  , HasAnn stmt Lexer.SourceRegion
  , HasAnn simPat Lexer.SourceRegion
  , HasAnn ty Lexer.SourceRegion
  , HasAnn expr Lexer.SourceRegion
  ) =>
  Parser (mod Lexer.SourceRegion) ->
  Parser (stmt Lexer.SourceRegion) ->
  Parser (simPat Lexer.SourceRegion) ->
  Parser (ty Lexer.SourceRegion) ->
  Parser (expr Lexer.SourceRegion) ->
  Parser (Surface.ModF mod stmt simPat ty expr Lexer.SourceRegion, Lexer.SourceRegion)
pModDefinition pMod' pStmt pSimPat pTy pExpr = do
  modTok <- single (Lexer.Keyword Lexer.Mod)
  ident <- moduleIdentifier
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  body <- pModDefinitionBody pMod' pStmt pSimPat pTy pExpr
  tokE <- single (Lexer.Punctuation Lexer.RightBrace)
  let ann = Lexer.SourceRegion{start = modTok.region.start, end = tokE.region.end}
  pure (Surface.ModDefinitionF ident body, ann)

pPubUse ::
  Parser
    ( Surface.Pub Lexer.SourceRegion
    , Surface.UseClause Lexer.SourceRegion
    , Lexer.SourceRegion
    )
pPubUse = do
  (pub, pubAnn) <- pPub
  use <- pUseClause
  pure (pub, use, Lexer.SourceRegion{start = pubAnn.start, end = use.ann.end})
