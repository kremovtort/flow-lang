module Flow.AST.Surface.Literal where

import "scientific" Data.Scientific (Scientific)
import "text" Data.Text (Text)
import "bytestring" Data.ByteString (ByteString)
import "base" Data.Word (Word8)

data Literal
  = LitBool Bool
  | LitInteger Integer
  | LitFloat Scientific
  | LitByte Word8
  | LitByteString ByteString
  | LitChar Char
  | LitString Text
  deriving (Eq, Ord, Show)
