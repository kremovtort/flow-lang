{-# OPTIONS_GHC -Wno-orphans #-}
module Flow.Core.Module where

import "base" GHC.Generics (Generic)
import "hashable" Data.Hashable (Hashable (..))
import "text" Data.Text (Text)
import "vector" Data.Vector (Vector)
import "base" Data.Foldable (toList)

import Flow.Core.Package (PackageId)
import Data.HashMap.Strict (HashMap)

instance (Hashable a) => Hashable (Vector a) where
  hashWithSalt salt = hashWithSalt salt . toList

data ModuleId = ModuleId
  { package :: PackageId
  , name :: Vector Text
  }
  deriving (Eq, Ord, Show, Hashable, Generic)


newtype ModuleEnv = ModuleEnv
  { modules :: HashMap ModuleId ()
  }
  deriving (Eq, Ord, Show)
