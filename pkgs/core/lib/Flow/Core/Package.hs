module Flow.Core.Package where

import "base" GHC.Generics (Generic)
import "hashable" Data.Hashable (Hashable)
import "text" Data.Text (Text)

newtype PackageId = PackageId
  { name :: Text
  }
  deriving (Eq, Ord, Show, Hashable, Generic)
