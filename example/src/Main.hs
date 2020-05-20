module Main where

import qualified Text
import qualified Task
import qualified Debug
import qualified Interop
import qualified List
import qualified Dict
import qualified Data.Text
import qualified Log
import qualified Control.Concurrent
import qualified Control.Exception.Safe as Exception
import qualified GHC.Stack as Stack
import qualified Network.HTTP as HTTP
import qualified System.IO
import qualified Json.Decode as Json
import Control.Monad (void)
import Task (Task)
import Log hiding (tracer)
import Cherry.Prelude
import Json.Decode ()
import Json.Encode ()
import qualified Prelude
import qualified Data.Time.Clock as Clock


-- json clean up
-- string -> text


main :: Prelude.IO ()
main = do
  interop <- Interop.key
  res <- Task.attempt (Just (tracer interop)) (targets interop) (application interop)
  case res of
    Ok a -> Prelude.putStrLn "Done!"
    Err e -> Prelude.putStrLn <| "Not good" ++ e


tracer :: Interop.Key -> Log.Tracer
tracer interop =
  let before _ context = do
        time <- Clock.getCurrentTime |> Interop.enter interop |> Task.mapError Prelude.show
        case Json.fromValue decoder context of
          Ok True -> print_ interop "Online!"
          Ok False -> print_ interop "Not online!"
          Err _ -> print_ interop "Sad."
        Task.succeed time

      decoder =
        Json.field "online" Json.bool

      after stuff = do
        time <- Clock.getCurrentTime |> Interop.enter interop |> Task.mapError Prelude.show
        print_ interop (Debug.toString stuff ++ " -> " ++ Debug.toString time ++ "\n")
  in
  Log.tracer before after



-- APP


application :: Interop.Key -> Task Prelude.String ()
application interop = do
  segment "app" [] <| do
    good interop "> hello 1"
    debug
      [ text "user" "tereza", text "email" "terezasokol@gmail.com" ]
      "LOG 1. This is a really long message\n This is a really long message This is a really long message This is a really long message This is a really long message This is a really long message"
    good interop "> hello 2"
    bad interop "> hello 3"
    good interop "> hello 4"
    info [] "LOG 3."
    --Prelude.error "this is bad"
    --  |> Interop.enter
    debug [] "LOG 4."


good :: Interop.Key -> Text -> Task Prelude.String ()
good interop string = do
  segment "good" [ value "online" False ] <| do
    print_ interop (string ++ "\n")


bad :: Interop.Key -> Text -> Task Prelude.String ()
bad interop string = do
  segment "bad" [ value "is_ok" (12.1 :: Float) ] <| do
    Task.succeed ()
    Prelude.error "this is fine"
      |> Interop.enter interop
      |> Task.onError (Log.exception [ text "library" "bad-library" ])
    error [] "LOG 2."
    -- Interop.enter <| Prelude.error "hello!!"


{-| -}
print_ :: Interop.Key -> Text.Text -> Task Prelude.String ()
print_ interop string =
  Prelude.putStr (Data.Text.unpack string)
    |> Interop.enter interop
    |> Task.mapError Prelude.show



-- LOGGING


targets :: Interop.Key -> List Target
targets interop =
  [ file "log.txt" compact
  , terminal pretty
  , bugsnag interop
  ]


bugsnag :: Interop.Key -> Target
bugsnag interop =
  let write :: Entry -> Task Prelude.String ()
      write (Entry{context = context}) = do
        --Prelude.error "bugsnag failed"
        --  |> Interop.enter Prelude.show
        Control.Concurrent.threadDelay 1000000
          |> Interop.enter interop
          |> Task.mapError Prelude.show
        HTTP.simpleHTTP (HTTP.getRequest "http://hackage.haskell.org/")
          |> Interop.enter interop
          |> Task.mapError Prelude.show
          |> Task.andThen (print context)

      print context _ =
        case Log.lookup "user" context :: Result Text Text of
          Ok "tereza" -> print_ interop "tereza\n"
          _ -> print_  interop"no user\n"
  in
  custom write
