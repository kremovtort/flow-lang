{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use first" #-}
module Flow.Parser.Module where

import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "vector" Data.Vector qualified as Vector

import Flow.AST.Surface qualified as Surface
import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Module qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (Parser, methodIdentifier, moduleIdentifier, simpleTypeIdentifier, simpleVarIdentifier, single)
import Flow.Parser.Decl (pPub)

pModDefinitionBody :: Parser (Surface.ModDefinitionBody Lexer.SourceRegion)
pModDefinitionBody = do
  uses <- Megaparsec.many pUseClause
  pure $
    Surface.ModDefinitionBodyF
      { uses = Vector.fromList uses
      , items = mempty
      }

pModDefinition :: Parser (Surface.Mod Lexer.SourceRegion)
pModDefinition = fail "Flow.Parser.Module.pModDefinition: not implemented"

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
