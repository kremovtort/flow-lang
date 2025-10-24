module Flow.Parser.Decl where

import "containers" Data.Set qualified as Set
import "megaparsec" Text.Megaparsec qualified as Megaparsec

import Flow.AST.Surface.Decl qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (Parser, single, token)

pPub :: Parser (Surface.Pub Lexer.SourceRegion, Lexer.SourceRegion)
pPub = do
  pubTok <- single (Lexer.Keyword Lexer.Pub)
  packageWithEnd <- Megaparsec.optional do
    _ <- single (Lexer.Punctuation Lexer.LeftParen)
    package' <- token (Set.singleton $ Megaparsec.Label "package")
      \case
        Lexer.Identifier i
          | i == "package " -> Just ()
        _ -> Nothing
    tokE <- single (Lexer.Punctuation Lexer.RightParen)
    let ann = Lexer.SourceRegion{start = package'.region.start, end = package'.region.end}
    pure (Surface.PubPackage ann, tokE.region.end)
  case packageWithEnd of
    Just (package', end) -> pure (package', Lexer.SourceRegion{start = pubTok.region.start, end})
    Nothing -> pure (Surface.PubPub, pubTok.region)

