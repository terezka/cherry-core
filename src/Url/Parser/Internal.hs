module Url.Parser.Internal
  ( QueryParser(..)
  ) where


import Cherry.Prelude
import qualified Prelude
import qualified Dict
import qualified List


newtype QueryParser a =
  Parser (Dict.Dict String (List String) -> a)