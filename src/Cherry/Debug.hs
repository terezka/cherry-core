-- | This module can be useful while _developing_ an application. It is not
-- available for use in packages or production.
--
-- Debugging
-- @docs toString, log, todo
module Cherry.Debug
  ( toString,
    log,
    todo,
  )
where

import Prelude (Show, error, show)
import Data.Text (pack, unpack)
import Cherry.Basics ((>>))
import Cherry.Text (Text, concat)
import qualified Debug.Trace


{-| Turn any kind of value into a string.

  >  toString 42                == "42"
  >  toString [1,2]             == "[1,2]"
  >  toString ('a', "cat", 13)  == "('a', \"cat\", 13)"
  >  toString "he said, \"hi\"" == "\"he said, \\\"hi\\\"\""

-}
toString :: Show a => a -> Text
toString =
  show >> pack


{-| Log a tagged value on the developer console, and then return the value.

  >  1 + log "number" 1        -- equals 2, logs "number: 1"
  >  length (log "start" [])   -- equals 0, logs "start: []"

It is often possible to sprinkle this around to see if values are what you
expect. It is kind of old-school to do it this way, but it works!

-}
log :: Show a => Text -> a -> a
log message value =
  Debug.Trace.trace (unpack (concat [message, ": ", toString value])) value


{-| This is a placeholder for code that you will write later.

For example, if you are working with a large union type and have partially
completed a case expression, it may make sense to do this:

  >  type Entity = Ship | Fish | Captain | Seagull
  >
  >  drawEntity entity =
  >    case entity of
  >      Ship ->
  >        ...
  >
  >      Fish ->
  >        ...
  >
  >      _ ->
  >        Debug.todo "handle Captain and Seagull"
  >

-}
todo :: Text -> a
todo =
  unpack >> error
