module Json.Ast (AST(..)) where

import qualified Data.ByteString.Internal as ByteString
import Basics


data AST
  = Array [AST]
  | Object [(ByteString.ByteString, AST)]
  | String ByteString.ByteString
  | Int Int
  | Float Float
  | Boolean Bool
  | NULL