{-# LANGUAGE RankNTypes #-}

{-|

Module      : Basics
Description : Basics for working with Cherry.
License     : BSD 3
Maintainer  : terezasokol@gmail.com
Stability   : experimental
Portability : POSIX

-}

module Basics
  ( -- * Math
    Int, Float, (+), (-), (*), (/), (//), (^)

    -- * Int to Float / Float to Int
  , toFloat, round, floor, ceiling, truncate

    -- * Equality
  , (==), (/=)

    -- * Comparison
  , (<), (>), (<=), (>=), max, min, compare, Order(..)

    -- * Booleans
  , Bool(..), not, (&&), (||)

    -- * Append Strings and Lists
  , Appendable, (++)

    -- * Fancier Math
  , modBy, remainderBy, negate, abs, clamp, sqrt, logBase

    -- * Trigonometry
  , pi, cos, sin, tan, acos, asin, atan, atan2

    -- * Floating Point Checks
  , isNaN, isInfinite

    -- * Function Helpers
  , identity, always, (<|), (|>), (<<), (>>), Never, never
  ) where

import Prelude (Bool)
import qualified Prelude
import qualified List
import qualified String


-- INFIX OPERATORS


infixr 0  <|
infixl 0  |>
infixr 2  ||
infixr 3  &&
infix  4  ==, /=, <, >, <=, >=
infixr 5  ++
infixl 6  +, -
infixl 7  *, /, //
infixr 8  ^
infixl 9  <<
infixr 9  >>



-- MATHEMATICS


{-| An `Int` is a whole number. Valid syntax for integers includes:

  >  0
  >  42
  >  9000
  >  0xFF   -- 255 in hexadecimal
  >  0x000A --  10 in hexadecimal

Historical Note: The name `Int` comes from the term [integer](https://en.wikipedia.org/wiki/Integer). It appears
that the `int` abbreviation was introduced in [ALGOL 68](https://en.wikipedia.org/wiki/ALGOL_68), shortening it
from `integer` in [ALGOL 60](https://en.wikipedia.org/wiki/ALGOL_60). Today, almost all programming languages use
this abbreviation.

-}
type Int = Prelude.Int


{-| A `Float` is a [floating-point number](https://en.wikipedia.org/wiki/Floating-point_arithmetic). Valid syntax for floats includes:

  >  0
  >  42
  >  3.14
  >  0.1234
  >  6.022e23   -- == (6.022 * 10^23)
  >  6.022e+23  -- == (6.022 * 10^23)
  >  1.602e−19  -- == (1.602 * 10^-19)
  >  1e3        -- == (1 * 10^3) == 1000

Historical Note: The particular details of floats (e.g. `NaN`) are
specified by [IEEE 754](https://en.wikipedia.org/wiki/IEEE_754) which is literally hard-coded into almost all
CPUs in the world. That means if you think `NaN` is weird, you must
successfully overtake Intel and AMD with a chip that is not backwards
compatible with any widely-used assembly language.

-}
type Float = Prelude.Double



{-| Add two numbers. The `number` type variable means this operation can be
specialized to `Int -> Int -> Int` or to `Float -> Float -> Float`. So you
can do things like this:

  >  3002 + 4004 == 7006  -- all ints
  >  3.14 + 3.14 == 6.28  -- all floats

You _cannot_ add an `Int` and a `Float` directly though. Use functions like
`toFloat` or `round` to convert both values to the same type.
So if you needed to add a list length to a `Float` for some reason, you
could say one of these:

  >  3.14 + toFloat (List.length [1,2,3]) == 6.14
  >  round 3.14 + List.length [1,2,3]     == 6

-}
(+) :: Prelude.Num number => number -> number -> number
(+) =
  (Prelude.+)


{-| Subtract numbers like `4 - 3 == 1`.

See `(+)` for docs on the `number` type variable.
-}
(-) :: (Prelude.Num number) => number -> number -> number
(-) =
  (Prelude.-)


{-| Multiply numbers like `2 * 3 == 6`.

See `(+)` for docs on the `number` type variable.
-}
(*) :: (Prelude.Num number) => number -> number -> number
(*) =
  (Prelude.*)


{-| Floating-point division:

  >  3.14 / 2 == 1.57
-}
(/) :: Float -> Float -> Float
(/) =
  (Prelude./)


{-| Integer division:

  >  3 // 2 == 1

Notice that the remainder is discarded.
-}
(//) :: Int -> Int -> Int
(//) =
  Prelude.quot


{-| Exponentiation

  >  3^2 == 9
  >  3^3 == 27
-}
(^) :: (Prelude.Num number, Prelude.Integral number) => number -> number -> number
(^) =
  (Prelude.^)



-- INT TO FLOAT / FLOAT TO INT


{-| Convert an integer into a float. Useful when mixing `Int` and `Float`
values like this:

  >  halfOf :: Int -> Float
  >  halfOf number =
  >    toFloat number / 2

-}
toFloat :: Int -> Float
toFloat x =
  Prelude.fromIntegral x :: Float


{-| Round a number to the nearest integer.

  >  round 1.0 == 1
  >  round 1.2 == 1
  >  round 1.5 == 2
  >  round 1.8 == 2
  >  round -1.2 == -1
  >  round -1.5 == -1
  >  round -1.8 == -2

-}
round :: Float -> Int
round =
  Prelude.round


{-| Floor function, rounding down.

  >  floor 1.0 == 1
  >  floor 1.2 == 1
  >  floor 1.5 == 1
  >  floor 1.8 == 1
  >  floor -1.2 == -2
  >  floor -1.5 == -2
  >  floor -1.8 == -2

-}
floor :: Float -> Int
floor =
  Prelude.floor


{-| Ceiling function, rounding up.

  >  ceiling 1.0 == 1
  >  ceiling 1.2 == 2
  >  ceiling 1.5 == 2
  >  ceiling 1.8 == 2
  >  ceiling -1.2 == -1
  >  ceiling -1.5 == -1
  >  ceiling -1.8 == -1

-}
ceiling :: Float -> Int
ceiling =
  Prelude.ceiling


{-| Truncate a number, rounding towards zero.

  >  truncate 1.0 == 1
  >  truncate 1.2 == 1
  >  truncate 1.5 == 1
  >  truncate 1.8 == 1
  >  truncate -1.2 == -1
  >  truncate -1.5 == -1
  >  truncate -1.8 == -1

-}
truncate :: Float -> Int
truncate =
  Prelude.truncate



-- EQUALITY


{-| Check if values are &ldquo;the same&rdquo;.

-}
(==) :: (Prelude.Eq a) => a -> a -> Bool
(==) =
  (Prelude.==)


{-| Check if values are not &ldquo;the same&rdquo;.

So `(a /= b)` is the same as `(not (a == b))`.
-}
(/=) :: (Prelude.Eq a) => a -> a -> Bool
(/=) =
  (Prelude./=)



-- COMPARISONS


{-|-}
(<) :: (Prelude.Ord comparable) => comparable -> comparable -> Bool
(<) =
  (Prelude.<)


{-|-}
(>) :: (Prelude.Ord comparable) => comparable -> comparable -> Bool
(>) =
  (Prelude.>)


{-|-}
(<=) :: (Prelude.Ord comparable) => comparable -> comparable -> Bool
(<=) =
  (Prelude.<=)


{-|-}
(>=) :: (Prelude.Ord comparable) => comparable -> comparable -> Bool
(>=) =
  (Prelude.>=)


{-| Find the smaller of two comparables.

  >  min 42 12345678 == 42
  >  min "abc" "xyz" == "abc"

-}
min :: (Prelude.Ord comparable) => comparable -> comparable -> comparable
min =
  Prelude.min


{-| Find the larger of two comparables.

  >  max 42 12345678 == 12345678
  >  max "abc" "xyz" == "xyz"

-}
max :: (Prelude.Ord comparable) => comparable -> comparable -> comparable
max =
  Prelude.max


{-| Compare any two comparable values. Comparable values include `String`,
`Char`, `Int`, `Float`, or a list or tuple containing comparable values. These
are also the only values that work as `Dict` keys or `Set` members.

  >  compare 3 4 == LT
  >  compare 4 4 == EQ
  >  compare 5 4 == GT

-}
compare :: Prelude.Ord comparable => comparable -> comparable -> Order
compare =
  Prelude.compare


{-| Represents the relative ordering of two things.

The relations are less than, equal to, and greater than.
-}
type Order = Prelude.Ordering



-- BOOLEANS


{-| Negate a boolean value.

  >  not True == False
  >  not False == True

-}
not :: Bool -> Bool
not =
  Prelude.not


{-| The logical AND operator. `True` if both inputs are `True`.

  >  True  && True  == True
  >  True  && False == False
  >  False && True  == False
  >  False && False == _False

Note:_ When used in the infix position, like `(left && right)`, the operator
short-circuits. This means if `left` is `False` we do not bother evaluating `right`
and just return `False` overall.

-}
(&&) :: Bool -> Bool -> Bool
(&&) =
  (Prelude.&&)


{-| The logical OR operator. `True` if one or both inputs are `True`.

  >  True  || True  == True
  >  True  || False == True
  >  False || True  == True
  >  False || False == False

Note: When used in the infix position, like `(left || right)`, the operator
short-circuits. This means if `left` is `True` we do not bother evaluating `right`
and just return `True` overall.

-}
(||) :: Bool -> Bool -> Bool
(||) =
  (Prelude.||)



-- APPEND


{-| Put two appendable things together. This includes strings and lists.

  >  "hello" ++ "world" == "helloworld"
  >  [1,1,2] ++ [3,5,8] == [1,1,2,3,5,8]

-}
(++) :: (Appendable appendable) => appendable -> appendable -> appendable
(++) =
  append


class Appendable a where
  append :: a -> a -> a


instance Appendable String.String where
  append = String.append


instance Appendable [a] where
  append = List.append



-- FANCIER MATH


{-| Perform [modular arithmetic](https://en.wikipedia.org/wiki/Modular_arithmetic).
A common trick is to use (n mod 2) to detect even and odd numbers:

  >  modBy 2 0 == 0
  >  modBy 2 1 == 1
  >  modBy 2 2 == 0
  >  modBy 2 3 == 1

Our `modBy` function works in the typical mathematical way when you run into
negative numbers:

  >  List.map (modBy 4) [ -5, -4, -3, -2, -1,  0,  1,  2,  3,  4,  5 ]
  >  --                 [  3,  0,  1,  2,  3,  0,  1,  2,  3,  0,  1 ]

Use `remainderBy` for a different treatment of negative numbers,
or read Daan Leijen’s [Division and Modulus for Computer Scientists](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf) for more
information.

-}
modBy :: Int -> Int -> Int
modBy modulus n =
  n `Prelude.mod` modulus


{-| Get the remainder after division. Here are bunch of examples of dividing by four:

  >  List.map (remainderBy 4) [ -5, -4, -3, -2, -1,  0,  1,  2,  3,  4,  5 ]
  >  --                       [ -1,  0, -3, -2, -1,  0,  1,  2,  3,  0,  1 ]

Use `modBy` for a different treatment of negative numbers,
or read Daan Leijen’s [Division and Modulus for Computer Scientists](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf) for more
information.

-}
remainderBy :: Int -> Int -> Int
remainderBy divisor numerator =
  numerator `Prelude.rem` divisor


{-| Negate a number.

  >  negate 42 == -42
  >  negate -42 == 42
  >  negate 0 == 0

-}
negate :: (Prelude.Num number) => number -> number
negate =
  Prelude.negate


{-| Get the [absolute value](https://en.wikipedia.org/wiki/Absolute_value) of a number.

  >  abs 16   == 16
  >  abs -4   == 4
  >  abs -8.5 == 8.5
  >  abs 3.14 == 3.14

-}
abs :: (Prelude.Num number) => number -> number
abs =
  Prelude.abs


{-| Clamps a number within a given range. With the expression
`clamp 100 200 x` the results are as follows:

  >  100     if x < 100
  >   x      if 100 <= x < 200
  >  200     if 200 <= x

-}
clamp :: (Prelude.Num number, Prelude.Ord number) => number -> number -> number -> number
clamp low high number =
  if number < low then
    low
  else if number > high then
    high
  else
    number


{-| Take the square root of a number.

  >  sqrt  4 == 2
  >  sqrt  9 == 3
  >  sqrt 16 == 4
  >  sqrt 25 == 5

-}
sqrt :: Float -> Float
sqrt =
  Prelude.sqrt


{-| Calculate the logarithm of a number with a given base.

  >  logBase 10 100 == 2
  >  logBase 2 256 == 8

-}
logBase :: Float -> Float -> Float
logBase base number =
  Prelude.log number / Prelude.log base



-- TRIGONOMETRY


{-| An approximation of pi.

-}
pi :: Float
pi =
  Prelude.pi


{-| Figure out the cosine given an angle in radians.

  >  cos (degrees 60)     == 0.5000000000000001
  >  cos (turns (1/6))    == 0.5000000000000001
  >  cos (radians (pi/3)) == 0.5000000000000001
  >  cos (pi/3)           == 0.5000000000000001

-}
cos :: Float -> Float
cos =
  Prelude.cos


{-| Figure out the sine given an angle in radians.

  >  sin (degrees 30)     == 0.49999999999999994
  >  sin (turns (1/12))   == 0.49999999999999994
  >  sin (radians (pi/6)) == 0.49999999999999994
  >  sin (pi/6)           == 0.49999999999999994

-}
sin :: Float -> Float
sin =
  Prelude.sin


{-| Figure out the tangent given an angle in radians.

  >  tan (degrees 45)     == 0.9999999999999999
  >  tan (turns (1/8))    == 0.9999999999999999
  >  tan (radians (pi/4)) == 0.9999999999999999
  >  tan (pi/4)           == 0.9999999999999999

-}
tan :: Float -> Float
tan =
  Prelude.tan


{-| Figure out the arccosine for `adjacent / hypotenuse` in radians:

  >  acos (1/2) == 1.0471975511965979 -- 60° or pi/3 radians

-}
acos :: Float -> Float
acos =
  Prelude.acos


{-| Figure out the arcsine for `opposite / hypotenuse` in radians:

  >  asin (1/2) == 0.5235987755982989 -- 30° or pi/6 radians

-}
asin :: Float -> Float
asin =
  Prelude.asin


{-| This helps you find the angle (in radians) to an `(x,y)` coordinate, but
in a way that is rarely useful in programming. _You probably want
`atan2` instead!_

This version takes `y/x` as its argument, so there is no way to know whether
the negative signs comes from the `y` or `x` value. So as we go counter-clockwise
around the origin from point `(1,1)` to `(1,-1)` to `(-1,-1)` to `(-1,1)` we do
not get angles that go in the full circle:

  >  atan (  1 /  1 ) ==  0.7853981633974483 --  45° or   pi/4 radians
  >  atan (  1 / -1 ) == -0.7853981633974483 -- 315° or 7*pi/4 radians
  >  atan ( -1 / -1 ) ==  0.7853981633974483 --  45° or   pi/4 radians
  >  atan ( -1 /  1 ) == -0.7853981633974483 -- 315° or 7*pi/4 radians

Notice that everything is between `pi/2` and `-pi/2`. That is pretty useless
for figuring out angles in any sort of visualization, so again, check out
`atan2` instead!

-}
atan :: Float -> Float
atan =
  Prelude.atan


{-| This helps you find the angle (in radians) to an `(x,y)` coordinate.
So rather than saying `atan (y/x)` you say `atan2 y x` and you can get a full
range of angles:

  >  atan2  1  1 ==  0.7853981633974483 --  45° or   pi/4 radians
  >  atan2  1 -1 ==  2.356194490192345  -- 135° or 3*pi/4 radians
  >  atan2 -1 -1 == -2.356194490192345  -- 225° or 5*pi/4 radians
  >  atan2 -1  1 == -0.7853981633974483 -- 315° or 7*pi/4 radians

-}
atan2 :: Float -> Float -> Float
atan2 =
  Prelude.atan2



-- CRAZY FLOATS


{-| Determine whether a float is an undefined or unrepresentable number.
NaN stands for *not a number* and it is [a standardized part of floating point
numbers](https://en.wikipedia.org/wiki/NaN).

  >  isNaN (0/0)     == True
  >  isNaN (sqrt -1) == True
  >  isNaN (1/0)     == False  -- infinity is a number
  >  isNaN 1         == False

-}
isNaN :: Float -> Bool
isNaN =
  Prelude.isNaN


{-| Determine whether a float is positive or negative infinity.

  >  isInfinite (0/0)     == False
  >  isInfinite (sqrt -1) == False
  >  isInfinite (1/0)     == True
  >  isInfinite 1         == False

Notice that NaN is not infinite! For float `n` to be finite implies
that `not (isInfinite n || isNaN n)` evaluates to `True`.
-}
isInfinite :: Float -> Bool
isInfinite =
  Prelude.isInfinite



-- FUNCTION HELPERS


{-| Function composition, passing results along in the suggested direction. For
example, the following code checks if the square root of a number is odd:

  >  not << isEven << sqrt

You can think of this operator as equivalent to the following:

  >  (g << f)  ==  (\x -> g (f x))

So our example expands out to something like this:

  >  \n -> not (isEven (sqrt n))

-}
(<<) :: (b -> c) -> (a -> b) -> (a -> c)
(<<) g f x =
  g (f x)


{-| Function composition, passing results along in the suggested direction. For
example, the following code checks if the square root of a number is odd:

  >  sqrt >> isEven >> not

-}
(>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>) f g x =
  g (f x)


{-| Saying `x |> f` is exactly the same as `f x`.
It is called the “pipe” operator because it lets you write “pipelined” code.
For example, say we have a `sanitize` function for turning user input into
integers:

  >  -- BEFORE
  >  sanitize :: String -> Maybe Int
  >  sanitize input =
  >    String.toInt (String.trim input)

We can rewrite it like this:

  >  -- AFTER
  >  sanitize :: String -> Maybe Int
  >  sanitize input =
  >    input
  >      |> String.trim
  >      |> String.toInt

Totally equivalent! I recommend trying to rewrite code that uses `x |> f`
into code like `f x` until there are no pipes left. That can help you build
your intuition.

Note: This can be overused! I think folks find it quite neat, but when you
have three or four steps, the code often gets clearer if you break out a
top-level helper function. Now the transformation has a name. The arguments are
named. It has a type annotation. It is much more self-documenting that way!
Testing the logic gets easier too. Nice side benefit!

-}
(|>) :: a -> (a -> b) -> b
(|>) x f =
  f x


{-| Saying `f <| x` is exactly the same as `f x`.

It can help you avoid parentheses, which can be nice sometimes. Maybe you want
to apply a function to a `case` expression? That sort of thing.

-}
(<|) :: (a -> b) -> a -> b
(<|) f x =
  f x


{-| Given a value, returns exactly the same value. This is called
[the identity function](https://en.wikipedia.org/wiki/Identity_function).

-}
identity :: a -> a
identity x =
  x


{-| Create a function that always returns the same value. Useful with
functions like `map`:

  >  List.map (always 0) [1,2,3,4,5] == [0,0,0,0,0]
  >  -- List.map (\_ -> 0) [1,2,3,4,5] == [0,0,0,0,0]
  >  -- always = (\x _ -> x)

-}
always :: a -> b -> a
always a _ =
  a


{-| A value that can never happen! For context:

  - The boolean type `Bool` has two values: `True` and `False`
  - The unit type `()` has one value: `()`
  - The never type `Never` has no values!

The `Never` type is useful for restricting *arguments* to a function. Maybe my
API can only accept a result which never fails, so I require `Result Never a` and
users can give `Result msg` and everything will go fine. Generally speaking, you
do not want `Never` in your return types though.

-}
data Never
  = JustOneMore Never


{-| A function that can never be called. Seems extremely pointless, but it
*can* come in handy. Imagine you have some HTML that should never produce any
messages. And say you want to use it in some other HTML that *does* produce
messages. You could say:

  >  import Html exposing (..)
  >
  >  embedHtml :: Html Never -> Html msg
  >  embedHtml staticStuff =
  >    div []
  >      [ text "hello"
  >      , Html.map never staticStuff
  >      ]

So the `never` function is basically telling the type system, make sure no one
ever calls me!

-}
never :: Never -> a
never (JustOneMore nvr) =
  never nvr