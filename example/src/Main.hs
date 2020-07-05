{-# LANGUAGE GADTs, RankNTypes, FlexibleInstances, MultiParamTypeClasses #-}

module Main where

import qualified String
import qualified Debug
import qualified Interop
import qualified List
import qualified Terminal
import qualified Maybe
import qualified Dict
import qualified Data.Text
import qualified Log
import qualified Task
import qualified Control.Concurrent
import qualified Control.Exception.Safe as Exception
import qualified GHC.Stack as Stack
import qualified Network.HTTP as HTTP
import qualified System.IO
import qualified Json.Decode as Json
import qualified Json.Encode as Encode
import qualified Example.Log
import Control.Monad (void)
import Cherry.Prelude
import Example.Log (Ctx, setLevel, setName)
import qualified Prelude
import qualified Data.Time.Clock as Clock

-- better entry print
-- how to share task impl
-- tracer for all


main = do
  interop <- Interop.key
  res <- Task.attempt Log.basic (application interop)
  case res of
    Ok a -> Prelude.putStrLn "Done!"
    Err e -> Prelude.putStrLn ("Not good: " ++ e)



-- APP


application :: Interop.Key -> Task Log.Basic x ()
application interop = do
  segment "app" [ Log.int "level" 90 ] <| do
    Terminal.line "> hello 1"
    debug [] "Very first log"
    debug [ Log.int "level" 12 ] "LOG 1. This is a really long message\n This is a really long message This is a really long message This is a really long message This is a really long message This is a really long message"

    Control.Concurrent.threadDelay 1000000
          |> Interop.enter interop
          |> Task.onError (\_ -> Task.succeed ())

    book <-
      Task.fail "Could not find book."
        |> Task.onError (error [ Log.int "bookId" 2 ])

    Terminal.line "> hello 2"
    bad interop "> hello 3"
    Terminal.line "> hello 4"
    info [] "LOG 3."
    debug [] "LOG 4."


bad :: Interop.Key -> String -> Task Log.Basic x ()
bad interop _ = do
  segment "bad" [] <| do
    Prelude.error "this is fine"
      |> Interop.enter interop
      |> Task.onError (exception [ Log.bool "bad" True ])
    error [] "LOG 2."


data User
  = User String Int


encodeUser :: User -> Encode.Value
encodeUser (User name age) =
  Encode.object
    [ ( "name", Encode.string name )
    , ( "age", Encode.int age )
    ]


