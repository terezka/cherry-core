{-# LANGUAGE RankNTypes #-}

module Example.Log
  ( Ctx
  , setLevel, setName
  , bool, string, int, float, value
  , config
  ) where

import qualified String
import qualified Task
import qualified Debug
import qualified Interop
import qualified List
import qualified Terminal
import qualified Maybe
import qualified Dict
import qualified Data.Text
import qualified Log
import qualified Control.Concurrent
import qualified Json.Encode as Json
import qualified Data.Time.Clock as Clock
import Cherry.Prelude


data Ctx =
  Ctx
    { level :: Int
    , name :: Maybe String
    , misc :: Dict String Json.Value
    }


instance WithMisc Ctx where
  setMisc key value context =
    context { misc = Dict.insert key value (misc context) }


setLevel :: Int -> Ctx -> Ctx
setLevel level context =
  context { level = level }


setName :: String -> Ctx -> Ctx
setName name context =
  context { name = Just name }



-- CONFIG


config :: Interop.Key -> Task.Config Ctx
config interop =
  Task.Config
    { Task.init = Ctx { level = 0, name = Nothing, misc = Dict.empty }
    , Task.tracer = tracer interop
    , Task.targets = targets interop
    , Task.encoder = encoder
    }

encoder :: Ctx -> Json.Value
encoder context =
  Json.object
    [ ( "level", Json.int (level context) )
    , ( "name", Json.string (Maybe.withDefault "hello" <| name context) )
    , ( "misc", Json.dict identity identity (misc context) )
    ]



-- CONFIG / TRACER


tracer :: Interop.Key -> Log.Tracer Ctx
tracer interop =
  let tracer_ :: forall x a. String -> Ctx -> Task.Task Ctx x a -> Task.Task Ctx x a
      tracer_ namespace context task = do
        start <-
          Interop.enter interop Clock.getCurrentTime
            |> Task.map Debug.toString
            |> Task.onError (\_ -> Task.succeed "err")

        Terminal.line (if level context == 42 then "Online!" else "Sad.")

        result <- task

        end <-
          Interop.enter interop Clock.getCurrentTime
            |> Task.map Debug.toString
            |> Task.onError (\_ -> Task.succeed "err")

        Terminal.line (start ++ " -> " ++ end)

        Task.succeed result
  in
  Log.customTracer tracer_



-- CONFIG / TARGETS


targets :: Interop.Key -> List (Target Ctx)
targets interop =
  [ bugsnag interop
  , Log.terminal (Log.pretty encoder)
  --, Log.file "log.txt" Log.compact
  ]


bugsnag :: Interop.Key -> Target Ctx
bugsnag interop =
  let write :: Entry Ctx -> Task.Task s x ()
      write entry = do
        --Prelude.error "bugsnag failed"
        --  |> Interop.enter Prelude.show
        Control.Concurrent.threadDelay 1000000
          |> Interop.enter interop
          |> Task.onError (\_ -> Task.succeed ())

        print entry ()

      print entry _ =
        case name (Log.context entry) of
          Just user -> Terminal.line "has user"
          Nothing -> Terminal.line "no userdjflksjflkdsjf"
  in
  Log.target write
