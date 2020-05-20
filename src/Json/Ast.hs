module Json.Ast (AST(..)) where

import qualified Data.ByteString.Internal as ByteString
import Basics
import List (List)


data AST
  = Array (List AST)
  | Object (List ( ByteString.ByteString, AST ))
  | String ByteString.ByteString
  | Int Int
  | Float Float
  | Boolean Bool
  | NULL