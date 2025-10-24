{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use first" #-}
module Flow.Parser.Module where

import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "vector" Data.Vector qualified as Vector

import Flow.AST.Surface qualified as Surface
import Flow.AST.Surface.Module qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (Parser, moduleIdentifier, single)
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
      , Megaparsec.try pUseTreeLeafAs
      , Megaparsec.try pUseTreeLeafWildcard
      , pUseTreeLeafNamed
      ]
  pUseTreeBranch = do
    ident <- moduleIdentifier
    _ <- single (Lexer.Punctuation Lexer.ColonColon)
    tree <- pUseTree
    pure $ Surface.UseTreeBranch ident tree

  pUseTreeNested = do
    _ <- single (Lexer.Punctuation Lexer.LeftBrace)
    trees <- Megaparsec.sepEndBy pUseTree (single (Lexer.Punctuation Lexer.Comma))
    _ <- single (Lexer.Punctuation Lexer.RightBrace)
    pure $ Surface.UseTreeNested $ Vector.fromList trees

  pUseTreeLeafNamed = do
    ident <- moduleIdentifier
    pure $ Surface.UseTreeLeafNamed ident

  pUseTreeLeafWildcard = do
    tok <- single (Lexer.Punctuation Lexer.Star)
    pure $ Surface.UseTreeLeafWildcard tok.region

  pUseTreeLeafAs = do
    ident <- moduleIdentifier
    _ <- single (Lexer.Keyword Lexer.As)
    alias <- moduleIdentifier
    pure $ Surface.UseTreeLeafAs ident alias
