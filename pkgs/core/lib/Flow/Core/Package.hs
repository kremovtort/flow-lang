module Flow.Core.Package where

import "base" GHC.Generics (Generic)
import "hashable" Data.Hashable (Hashable)
import "text" Data.Text (Text)
import Data.TreeDiff (ToExpr)

newtype PackageId = PackageId
  { name :: Text
  }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, ToExpr)
