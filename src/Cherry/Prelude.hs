module Cherry.Prelude
  ( Task
  , module Cherry.Basics
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
import Cherry.Basics
import Cherry.Maybe (Maybe (..))
import Cherry.Result (Result (..))
import Cherry.Task (Task)
