module Flow.AST.Ann where

import "megaparsec" Text.Megaparsec (SourcePos)

-- | Common annotation for AST nodes: source span, extendable later with inferred types, scopes, etc.
data Ann = Ann
  { start :: SourcePos
  , end :: SourcePos
  }
  deriving (Eq, Ord, Show)
