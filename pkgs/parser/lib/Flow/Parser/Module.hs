{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use first" #-}
module Flow.Parser.Module where

import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "vector" Data.Vector qualified as Vector

import Flow.AST.Surface.Callable qualified as Surface
import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Constraint qualified as Surface
import Flow.AST.Surface.Decl qualified as Surface
import Flow.AST.Surface.Module qualified as Surface
import Flow.AST.Surface.Syntax qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Callable (pFnDefinition)
import Flow.Parser.Common (HasAnn, Parser, methodIdentifier, moduleIdentifier, simpleTypeIdentifier, simpleVarIdentifier, single)
import Flow.Parser.Constraint (pTypeDefinition)
import Flow.Parser.Decl (pEnum, pPub, pStruct)
import Flow.Parser.Syntax (pLetDefinition)

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

pUseClause :: Parser (Surface.UseClause Lexer.SourceRegion)
pUseClause = do
  pub <- Megaparsec.optional pPub
  useTok <- single (Lexer.Keyword Lexer.Use)
  root <- moduleIdentifier
  tree <- Megaparsec.optional do
    _ <- single (Lexer.Punctuation Lexer.ColonColon)
    pUseTree
  tokE <- single (Lexer.Punctuation Lexer.Semicolon)
  let start = case pub of
        Just (_, region) -> region.start
        Nothing -> useTok.region.start
  pure
    Surface.UseClause
      { pub = fmap fst pub
      , root
      , tree
      , ann = Lexer.SourceRegion{start, end = tokE.region.end}
      }
 where
  pUseTree =
    Megaparsec.choice
      [ Megaparsec.try pUseTreeNested
      , Megaparsec.try pUseTreeBranch
      , Megaparsec.try pUseTreeLeafMethodAsFn
      , Megaparsec.try pUseTreeLeafMethod
      , Megaparsec.try pUseTreeLeafVar
      , Megaparsec.try pUseTreeLeafType
      , Megaparsec.try pUseTreeLeafWildcard
      ]
  pUseTreeBranch = do
    ident <- moduleIdentifier
    _ <- single (Lexer.Punctuation Lexer.ColonColon)
    tree <- pUseTree
    pure $ Surface.UseTrBranch ident tree

  pUseTreeNested = do
    _ <- single (Lexer.Punctuation Lexer.LeftBrace)
    trees <- Megaparsec.sepEndBy pUseTree (single (Lexer.Punctuation Lexer.Comma))
    _ <- single (Lexer.Punctuation Lexer.RightBrace)
    pure $ Surface.UseTrNested $ Vector.fromList trees

  pUseTreeLeaf pIdent = do
    use <- pIdent
    as <- Megaparsec.optional do
      _ <- single (Lexer.Keyword Lexer.As)
      pIdent
    pure
      Surface.UseTreeLeaf
        { use = use
        , as = as
        , ann = case as of
            Nothing -> use.ann
            Just as' -> Lexer.SourceRegion use.ann.start as'.ann.end
        }

  pUseTreeLeafMethod = do
    leaf <- pUseTreeLeaf methodIdentifier
    pure $ Surface.UseTrLeafMethod leaf

  pUseTreeLeafVar = do
    leaf <- pUseTreeLeaf simpleVarIdentifier
    pure $ Surface.UseTrLeafVar leaf

  pUseTreeLeafType = do
    leaf <- pUseTreeLeaf simpleTypeIdentifier
    pure $ Surface.UseTrLeafType leaf

  pUseTreeLeafMethodAsFn = do
    use <- methodIdentifier
    _ <- single (Lexer.Keyword Lexer.As)
    as <- simpleVarIdentifier
    pure $
      Surface.UseTrLeafMethodAsFn
        Surface.UseTreeLeafMethodAsFn
          { use
          , as
          , ann = Lexer.SourceRegion use.ann.start as.ann.end
          }

  pUseTreeLeafWildcard = do
    tok <- single (Lexer.Punctuation Lexer.Star)
    pure $ Surface.UseTrLeafWildcard tok.region
