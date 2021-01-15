module Internal.Basics where

import qualified Prelude


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