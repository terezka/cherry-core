module Cherry.Prelude
  ( Task.Task
  , module Basics
  , Maybe (..)
  , Result (..)
  )
where

-- Elm implicitly imports a variety of names into each module. There isn't a
-- formal "prelude" like in Haskell; it's defined in the language. See
-- https://package.elm-lang.org/packages/elm/core/latest/ for the full list.
--
--   import Basics exposing (..)
--   import List exposing (List, (::))
--   import Maybe exposing (Maybe(..))
--   import Result exposing (Result(..))
--   import String exposing (String)
--   import Char exposing (Char)
--   import Tuple
--   import Debug
--
import Basics
import Maybe (Maybe (..))
import Nri.Prelude.Internal
import Nri.Prelude.Protolude
import Result (Result (..))
import qualified Task (Task)
