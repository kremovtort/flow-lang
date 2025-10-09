module Flow.AST.Core.Type.Common where

import "text" Data.Text (Text)

-- | Generic identifier name (types, classes, effects, etc.).
newtype Name = Name Text
  deriving (Eq, Ord, Show)

-- | RefScope identifier name (e.g., '\'s').
newtype ScopeName = ScopeName Text
  deriving (Eq, Ord, Show)


