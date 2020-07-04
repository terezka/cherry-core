{-# LANGUAGE GADTs, RankNTypes, FlexibleInstances, MultiParamTypeClasses #-}

module Main where

import qualified String
import qualified Task
import qualified Debug
import qualified Interop
import qualified List
import qualified Terminal
import qualified Dict
import qualified Data.Text
import qualified Log
import qualified Control.Concurrent
import qualified Control.Exception.Safe as Exception
import qualified GHC.Stack as Stack
import qualified Network.HTTP as HTTP
import qualified System.IO
import qualified Json.Decode as Json
import qualified Json.Encode as Encode
import Control.Monad (void)
import Cherry.Prelude
import qualified Prelude
import qualified Data.Time.Clock as Clock


-- better compact print

main = do
  interop <- Interop.key
  res <- Task.customAttempt (tracer interop) (targets interop) (application interop)
  case res of
    Ok a -> Prelude.putStrLn "Done!"
    Err e -> Prelude.putStrLn <| "Not good" ++ e


data User
  = User String Int

encodeUser :: User -> Encode.Value
encodeUser (User name age) =
  Encode.object
    [ ( "name", Encode.string name )
    , ( "age", Encode.int age )
    ]


tracer :: Interop.Key -> Log.Tracer
tracer interop =
  let tracer_ :: forall x a. String -> Dict String Encode.Value -> Task x a -> Task x a
      tracer_ namespace context task = do
        start <-
          Interop.enter interop Clock.getCurrentTime
            |> Task.map Debug.toString
            |> Task.onError (\_ -> Task.succeed "err")

        case Dict.get "online" context of
          Just _ -> Terminal.line "Online!"
          Nothing -> Terminal.line "Sad."

        result <- task

        end <-
          Interop.enter interop Clock.getCurrentTime
            |> Task.map Debug.toString
            |> Task.onError (\_ -> Task.succeed "err")

        Terminal.line (start ++ " -> " ++ end)

        Task.succeed result
  in
  Log.tracer tracer_



-- APP


application :: Interop.Key -> Task Prelude.String ()
application interop = do
  segment "app" [ int "hello" 23 ] <| do
    good interop "> hello 1"
    debug [ float "level" 4.20 ] "Hello"
    debug [ string "user" "tereza", string "email" "terezasokol@gmail.com", int "age" 24, value "user" (encodeUser <| User "hello" 23) ]
      "LOG 1. This is a really long message\n This is a really long message This is a really long message This is a really long message This is a really long message This is a really long message"

    Control.Concurrent.threadDelay 1000000
          |> Interop.enter interop
          |> Task.mapError Prelude.show
    good interop "> hello 2"
    bad interop "> hello 3"
    good interop "> hello 4"
    info [] "LOG 3."
    --Prelude.error "this is bad"
    --  |> Interop.enter
    debug [] "LOG 4."


good :: Interop.Key -> String -> Task Prelude.String ()
good interop string = do
  segment "good" [ bool "online" False ] <| do
    Terminal.line string


bad :: Interop.Key -> String -> Task Prelude.String ()
bad interop _ = do
  segment "bad" [ float "is_ok" 12.1 ] <| do
    Task.succeed ()
    Prelude.error "this is fine"
      |> Interop.enter interop
      |> Task.onError (Task.exception [ string "library" "bad-library" ])
    Task.error [] "LOG 2."
    -- Interop.enter <| Prelude.error "hello!!"



-- LOGGING


targets :: Interop.Key -> List Target
targets interop =
  [ bugsnag interop
  , Log.terminal Log.pretty
  , Log.file "log.txt" Log.compact
  ]


bugsnag :: Interop.Key -> Target
bugsnag interop =
  let write :: Entry -> Task Prelude.String ()
      write entry = do
        --Prelude.error "bugsnag failed"
        --  |> Interop.enter Prelude.show
        Control.Concurrent.threadDelay 1000000
          |> Interop.enter interop
          |> Task.mapError Prelude.show
        print entry ()

      print entry _ =
        case Dict.get "user" (Log.context entry) of
          Just user -> Terminal.line "has user"
          Nothing -> Terminal.line "no userdjflksjflkdsjf"
  in
  Log.target write
