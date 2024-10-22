
{-|

Module      : List
Description : Work with lists.
License     : BSD 3
Maintainer  : terezasokol@gmail.com
Stability   : experimental
Portability : POSIX

-}

module List
  ( -- * Create
    List, singleton, repeat, range

    -- * Transform
  , map, indexedMap, foldl, foldr, filter, filterMap

    -- * Utilities
  , length, reverse, member, all, any, maximum, minimum, sum, product

    -- * Combine
  , append, concat, concatMap, intersperse, map2, map3, map4, map5

    -- * Sort
  , sort, sortBy, sortWith

    -- * Deconstruct
  , isEmpty, head, tail, take, drop, partition, unzip
  )
where

import Prelude (Applicative, Char, Eq, Functor, Monad, Num, Ord, Show, Bool(..), Int, Ordering, (-), flip, mappend, mconcat)
import Maybe (Maybe (..))
import qualified Prelude
import qualified Data.List
import qualified Data.Maybe
import qualified Internal.Shortcut as Shortcut


{-| A list.
-}
type List a = [a]



-- CREATE


{-| Create a list with only one element:

  >  singleton 1234 == [1234]
  >  singleton "hi" == ["hi"]

-}
singleton :: a -> List a
singleton value =
  [value]


{-| Create a list with *n* copies of a value:

  >  repeat 3 (0,0) == [(0,0),(0,0),(0,0)]
-}
repeat :: Int -> a -> List a
repeat =
  Data.List.replicate


{-| Create a list of numbers, every element increasing by one.
You give the lowest and highest number that should be in the list.

  >  range 3 6 == [3, 4, 5, 6]
  >  range 3 3 == [3]
  >  range 6 3 == []
-}
range :: Int -> Int -> List Int
range lo hi =
  [lo .. hi]



-- TRANSFORM


{-| Apply a function to every element of a list.

  >  map sqrt [1,4,9] == [1,2,3]
  >
  >  map not [True,False,True] == [False,True,False]

So `map func [ a, b, c ]` is the same as `[ func a, func b, func c ]`
-}
map :: (a -> b) -> List a -> List b
map =
  Prelude.fmap


{-| Same as `map` but the function is also applied to the index of each
element (starting at zero).

  >  indexedMap Tuple.pair ["Tom","Sue","Bob"] == [ (0,"Tom"), (1,"Sue"), (2,"Bob") ]
-}
indexedMap :: (Int -> a -> b) -> List a -> List b
indexedMap f xs =
  List.map2 f [0 .. (length xs - 1)] xs


{-| Reduce a list from the left.

  >  foldl (+)  0  [1,2,3] == 6
  >  foldl (::) [] [1,2,3] == [3,2,1]

So 'foldl step state [1,2,3]' is like saying:

  >  state
  >    |> step 1
  >    |> step 2
  >    |> step 3
-}
foldl :: (a -> b -> b) -> b -> List a -> b
foldl func =
  -- Note: This function is implemented using fold' to eagerly evaluate the
  -- accumulator, preventing space leaks.
  Data.List.foldl' (flip func)


{-| Reduce a list from the right.

  >  foldr (+)  0  [1,2,3] == 6
  >  foldr (::) [] [1,2,3] == [1,2,3]

So `foldr step state [1,2,3]` is like saying:

  >  state
  >    |> step 3
  >    |> step 2
  >    |> step 1

-}
foldr :: (a -> b -> b) -> b -> List a -> b
foldr =
  Data.List.foldr


{-| Keep elements that satisfy the test.

  >  filter isEven [1,2,3,4,5,6] == [2,4,6]
-}
filter :: (a -> Bool) -> List a -> List a
filter =
  Data.List.filter


{-| Filter out certain values. For example, maybe you have a bunch of strings
from an untrusted source and you want to turn them into numbers:

  >  numbers :: List Int
  >  numbers =
  >    filterMap String.toInt ["3", "hi", "12", "4th", "May"]
  >
  >  -- numbers == [3, 12]

-}
filterMap :: (a -> Maybe b) -> List a -> List b
filterMap toMaybe =
  Data.Maybe.mapMaybe (\a -> toHMaybe (toMaybe a))



-- UTILITIES


{-| Determine the length of a list.

  >  length [1,2,3] == 3
-}
length :: List a -> Int
length =
  Data.List.length


{-| Reverse a list.

  >  reverse [1,2,3,4] == [4,3,2,1]
-}
reverse :: List a -> List a
reverse =
  Data.List.reverse


{-| Figure out whether a list contains a value.

  >  member 9 [1,2,3,4] == False
  >  member 4 [1,2,3,4] == True
-}
member :: Prelude.Eq a => a -> List a -> Bool
member =
  Data.List.elem

{-| Determine if all elements satisfy some test.

  >  all isEven [2,4] == True
  >  all isEven [2,3] == False
  >  all isEven [] == True
-}
all :: (a -> Bool) -> List a -> Bool
all =
  Data.List.all


{-| Determine if any elements satisfy some test.

  >  any isEven [2,3] == True
  >  any isEven [1,3] == False
  >  any isEven [] == False
-}
any :: (a -> Bool) -> List a -> Bool
any =
  Data.List.any


{-| Find the maximum element in a non-empty list.

  >  maximum [1,4,2] == Just 4
  >  maximum []      == Nothing
-}
maximum :: Prelude.Ord a => List a -> Maybe a
maximum list =
  case list of
    [] ->
      Nothing

    _ ->
      Just (Data.List.maximum list)


{-| Find the minimum element in a non-empty list.

  >  minimum [3,2,1] == Just 1
  >  minimum []      == Nothing
-}
minimum :: Prelude.Ord a => List a -> Maybe a
minimum list =
  case list of
    [] ->
      Nothing

    _ ->
      Just (Data.List.minimum list)


{-| Get the sum of the list elements.

  >  sum [1,2,3] == 6
  >  sum [1,1,1] == 3
  >  sum []      == 0

-}
sum :: Prelude.Num a => List a -> a
sum =
  Prelude.sum


{-| Get the product of the list elements.

  >  product [2,2,2] == 8
  >  product [3,3,3] == 27
  >  product []      == 1

-}
product :: Prelude.Num a => List a -> a
product =
  Prelude.product



-- COMBINE


{-| Put two lists together.

  >  append [1,1,2] [3,5,8] == [1,1,2,3,5,8]
  >  append ['a','b'] ['c'] == ['a','b','c']

You can also use [the `(++)` operator](Basics#++) to append lists.
-}
append :: List a -> List a -> List a
append =
  Prelude.mappend


{-| Concatenate a bunch of lists into a single list:

  >  concat [[1,2],[3],[4,5]] == [1,2,3,4,5]
-}
concat :: List (List a) -> List a
concat =
  Prelude.mconcat


{-| Map a given function onto a list and flatten the resulting lists.

  >  concatMap f xs == concat (map f xs)
-}
concatMap :: (a -> List b) -> List a -> List b
concatMap =
  Shortcut.andThen


{-| Places the given value between all members of the given list.

  >  intersperse "on" ["turtles","turtles","turtles"] == ["turtles","on","turtles","on","turtles"]
-}
intersperse :: a -> List a -> List a
intersperse =
  Data.List.intersperse


{-| Combine two lists, combining them with the given function.
If one list is longer, the extra elements are dropped.

  >  totals :: List Int -> List Int -> List Int
  >  totals xs ys =
  >    List.map2 (+) xs ys
  >
  >  -- totals [1,2,3] [4,5,6] == [5,7,9]
  >
  >  pairs :: List a -> List b -> List (a,b)
  >  pairs xs ys =
  >    List.map2 Tuple.pair xs ys
  >
  >  -- pairs ["alice","bob","chuck"] [2,5,7,8]
  >  --   == [("alice",2),("bob",5),("chuck",7)]

-}
map2 :: (a -> b -> result) -> List a -> List b -> List result
map2 =
  Data.List.zipWith


{-| -}
map3 :: (a -> b -> c -> result) -> List a -> List b -> List c -> List result
map3 =
  Data.List.zipWith3


{-| -}
map4 :: (a -> b -> c -> d -> result) -> List a -> List b -> List c -> List d -> List result
map4 =
  Data.List.zipWith4


{-| -}
map5 :: (a -> b -> c -> d -> e -> result) -> List a -> List b -> List c -> List d -> List e -> List result
map5 =
  Data.List.zipWith5



-- SORT


{-| Sort values from lowest to highest

    sort [3,1,5] == [1,3,5]
-}
sort :: Prelude.Ord a => List a -> List a
sort =
  Data.List.sort


{-| Sort values by a derived property.

  >  alice = { name="Alice", height=1.62 }
  >  bob   = { name="Bob"  , height=1.85 }
  >  chuck = { name="Chuck", height=1.76 }
  >
  >  sortBy .name   [chuck,alice,bob] == [alice,bob,chuck]
  >  sortBy .height [chuck,alice,bob] == [alice,chuck,bob]
  >
  >  sortBy String.length ["mouse","cat"] == ["cat","mouse"]
-}
sortBy :: Prelude.Ord b => (a -> b) -> List a -> List a
sortBy =
  Data.List.sortOn


{-| Sort values with a custom comparison function.

  >  sortWith flippedComparison [1,2,3,4,5] == [5,4,3,2,1]
  >
  >  flippedComparison a b =
  >      case compare a b of
  >        LT -> GT
  >        EQ -> EQ
  >        GT -> LT

This is also the most general sort function, allowing you
to define any other: `sort == sortWith compare`
-}
sortWith :: (a -> a -> Ordering) -> List a -> List a
sortWith =
  Data.List.sortBy



-- DECONSTRUCT


{-| Determine if a list is empty.

  >  isEmpty [] == True

Note: It is usually preferable to use a `case` to test this so you do not
forget to handle the `(x :: xs)` case as well!
-}
isEmpty :: List a -> Bool
isEmpty =
  Data.List.null


{-| Extract the first element of a list.

  >  head [1,2,3] == Just 1
  >  head [] == Nothing

Note: It is usually preferable to use a `case` to deconstruct a `List`
because it gives you `(x :: xs)` and you can work with both subparts.
-}
head :: List a -> Maybe a
head xs =
  case xs of
    x : _ ->
      Just x

    [] ->
      Nothing


{-| Extract the rest of the list.

  >  tail [1,2,3] == Just [2,3]
  >  tail [] == Nothing

Note: It is usually preferable to use a `case` to deconstruct a `List`
because it gives you `(x :: xs)` and you can work with both subparts.
-}
tail :: List a -> Maybe (List a)
tail list =
  case list of
    _ : xs ->
      Just xs

    [] ->
      Nothing


{-| Take the first *n* members of a list.

  >  take 2 [1,2,3,4] == [1,2]
-}
take :: Int -> List a -> List a
take =
  Data.List.take


{-| Drop the first *n* members of a list.

  >  drop 2 [1,2,3,4] == [3,4]
-}
drop :: Int -> List a -> List a
drop =
  Data.List.drop


{-| Partition a list based on some test. The first list contains all values
that satisfy the test, and the second list contains all the value that do not.

  >  partition (\x -> x < 3) [0,1,2,3,4,5] == ([0,1,2], [3,4,5])
  >  partition isEven        [0,1,2,3,4,5] == ([0,2,4], [1,3,5])
-}
partition :: (a -> Bool) -> List a -> (List a, List a)
partition =
  Data.List.partition


{-| Decompose a list of tuples into a tuple of lists.

  >  unzip [(0, True), (17, False), (1337, True)] == ([0,17,1337], [True,False,True])
-}
unzip :: List (a, b) -> (List a, List b)
unzip =
  Data.List.unzip



-- INTERNAL


toHMaybe :: Maybe a -> Data.Maybe.Maybe a
toHMaybe maybe =
  case maybe of
    Just a -> Data.Maybe.Just a
    Nothing -> Data.Maybe.Nothing
