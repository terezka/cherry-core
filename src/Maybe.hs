module Maybe
  ( -- This library fills a bunch of important niches in Elm. A `Maybe` can help
    -- you with optional arguments, error handling, and records with optional fields.

    -- * Definition
    Maybe(..)

    -- * Common Helpers
  , withDefault, map, map2, map3, map4, map5

    -- * Chaining Maybes
  , andThen
  )
where

import Prelude (Applicative, Char, Eq, Functor, Monad, Num, Ord, Show, flip, fromIntegral, mappend, mconcat, otherwise, pure)
import Data.Maybe (Maybe (..), fromMaybe)
import qualified Data.Maybe as Maybe
import qualified Cherry.Internal as Internal


{-| Represent values that may or may not exist. It can be useful if you have a
record field that is only filled in sometimes. Or if a function takes a value
sometimes, but does not absolutely need it.

  >  -- A person, but maybe we do not know their age.
  >  type alias Person =
  >      { name : String
  >      , age : Maybe Int
  >      }
  >
  >  tom = { name = "Tom", age = Just 42 }
  >  sue = { name = "Sue", age = Nothing }
-}
type Maybe a = Maybe.Maybe a


{-| Provide a default value, turning an optional value into a normal
value.  This comes in handy when paired with functions like
[`Dict.get`](Dict#get) which gives back a `Maybe`.

  >  withDefault 100 (Just 42)   -- 42
  >  withDefault 100 Nothing     -- 100
  >
  >  withDefault "unknown" (Dict.get "Tom" Dict.empty)   -- "unknown"

**Note:** This can be overused! Many cases are better handled by a `case`
expression. And if you end up using `withDefault` a lot, it can be a good sign
that a [custom type](https://guide.elm-lang.org/types/custom_types.html) will clean your code up quite a bit!
-}
withDefault :: a -> Maybe a -> a
withDefault =
  fromMaybe


{-| Transform a `Maybe` value with a given function:

  >  map sqrt (Just 9) == Just 3
  >  map sqrt Nothing  == Nothing

  >  map sqrt (String.toFloat "9") == Just 3
  >  map sqrt (String.toFloat "x") == Nothing

-}
map :: (a -> b) -> Maybe a -> Maybe b
map =
  Prelude.fmap


{-| Apply a function if all the arguments are `Just` a value.

  >  map2 (+) (Just 3) (Just 4) == Just 7
  >  map2 (+) (Just 3) Nothing == Nothing
  >  map2 (+) Nothing (Just 4) == Nothing
  >
  >  map2 (+) (String.toInt "1") (String.toInt "123") == Just 124
  >  map2 (+) (String.toInt "x") (String.toInt "123") == Nothing
  >  map2 (+) (String.toInt "1") (String.toInt "1.3") == Nothing
-}
map2 :: (a -> b -> value) -> Maybe a -> Maybe b -> Maybe value
map2 =
  Internal.map2

{-|-}
map3 :: (a -> b -> c -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe value
map3 =
  Internal.map3

{-|-}
map4 :: (a -> b -> c -> d -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe value
map4 =
  Internal.map4

{-|-}
map5 :: (a -> b -> c -> d -> e -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> Maybe value
map5 =
  Internal.map5

{-| Chain together many computations that may fail. It is helpful to see its
definition:

  >  andThen : (a -> Maybe b) -> Maybe a -> Maybe b
  >  andThen callback maybe =
  >      case maybe of
  >          Just value ->
  >              callback value
  >
  >          Nothing ->
  >              Nothing

This means we only continue with the callback if things are going well. For
example, say you need to parse some user input as a month:

  >  parseMonth : String -> Maybe Int
  >  parseMonth userInput =
  >      String.toInt userInput
  >        |> andThen toValidMonth
  >
  >  toValidMonth : Int -> Maybe Int
  >  toValidMonth month =
  >      if 1 <= month && month <= 12 then
  >          Just month
  >      else
  >          Nothing

In the `parseMonth` function, if `String.toInt` produces `Nothing` (because
the `userInput` was not an integer) this entire chain of operations will
short-circuit and result in `Nothing`. If `toValidMonth` results in `Nothing`,
again the chain of computations will result in `Nothing`.
-}
andThen :: (a -> Maybe b) -> Maybe a -> Maybe b
andThen =
  Internal.andThen
