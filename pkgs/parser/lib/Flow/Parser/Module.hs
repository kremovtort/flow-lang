{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Flow.Parser.Module where

import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "vector" Data.Vector qualified as Vector

import Flow.AST.Surface.Callable qualified as Surface
import Flow.AST.Surface.Constraint qualified as Surface
import Flow.AST.Surface.Decl qualified as Surface
import Flow.AST.Surface.Module qualified as Surface
import Flow.AST.Surface.Syntax qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Callable (pFnDefinition)
import Flow.Parser.Common (HasAnn, Parser, moduleIdentifier, pPub, single)
import Flow.Parser.Constraint (pTypeDefinition)
import Flow.Parser.Decl (pEnum, pStruct)
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
  Parser (Surface.ModDefinitionBodyF mod stmt simPat pat ty expr Lexer.SourceRegion)
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
  Parser (Surface.ModuleItemF mod stmt simPat pat ty expr Lexer.SourceRegion)
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
      , withRegion Surface.ModItemFnF <$> pFnDefinition pStmt pTy pExpr
      , withRegion Surface.ModItemLetF <$> pLetDefinition pSimPat pTy pExpr
      ]

  withRegion f item = (f item, item.ann)

pMod ::
  ( HasAnn mod Lexer.SourceRegion
  , HasAnn stmt Lexer.SourceRegion
  , HasAnn simPat Lexer.SourceRegion
  , HasAnn pat Lexer.SourceRegion
  , HasAnn ty Lexer.SourceRegion
  , HasAnn expr Lexer.SourceRegion
  ) =>
  Parser (mod Lexer.SourceRegion) ->
  Parser (stmt Lexer.SourceRegion) ->
  Parser (simPat Lexer.SourceRegion) ->
  Parser (pat Lexer.SourceRegion) ->
  Parser (ty Lexer.SourceRegion) ->
  Parser (expr Lexer.SourceRegion) ->
  Parser (Surface.ModF mod stmt simPat pat ty expr Lexer.SourceRegion, Lexer.SourceRegion)
pMod pMod' pStmt pSimPat pPat pTy pExpr =
  Megaparsec.choice [Megaparsec.try pModDeclaration, pModDefinition pMod' pStmt pSimPat pPat pTy pExpr]

pModDeclaration ::
  Parser (Surface.ModF mod lhsExpr simPat pat ty expr Lexer.SourceRegion, Lexer.SourceRegion)
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
  Parser (pat Lexer.SourceRegion) ->
  Parser (ty Lexer.SourceRegion) ->
  Parser (expr Lexer.SourceRegion) ->
  Parser (Surface.ModF mod stmt simPat pat ty expr Lexer.SourceRegion, Lexer.SourceRegion)
pModDefinition pMod' pStmt pSimPat pPat pTy pExpr = do
  modTok <- single (Lexer.Keyword Lexer.Mod)
  ident <- moduleIdentifier
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  body <- pModDefinitionBody pMod' pStmt pSimPat pTy pExpr
  tokE <- single (Lexer.Punctuation Lexer.RightBrace)
  let ann = Lexer.SourceRegion{start = modTok.region.start, end = tokE.region.end}
  pure (Surface.ModDefinitionF ident body, ann)
