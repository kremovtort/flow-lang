module Flow.AST.Ann where

import "megaparsec" Text.Megaparsec (SourcePos)

data SourceSpan = SourceSpan
  { start :: SourcePos
  , end :: SourcePos
  }
  deriving (Eq, Ord, Show)
