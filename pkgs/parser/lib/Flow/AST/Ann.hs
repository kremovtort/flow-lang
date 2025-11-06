module Flow.AST.Ann where

import "megaparsec" Text.Megaparsec (SourcePos)

data Ann = Ann
  { start :: SourcePos
  , end :: SourcePos
  }
  deriving (Eq, Ord, Show)
