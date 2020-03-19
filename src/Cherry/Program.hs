{-# LANGUAGE RankNTypes #-}

module Cherry.Program (Program, program) where

import qualified Cherry.Settings as Settings
import qualified Cherry.Task as Task
import qualified Cherry.Log as Log
import qualified Prelude
import Cherry.Text (Text)
import Cherry.Task (Task)
import Cherry.List (List)
import Cherry.Result (Result(..))
import Cherry.Basics


{-| -}
type Program =
  Prelude.IO ()


{-| -}
program :: forall settings keys. Settings.Decoder settings -> (settings -> Task Text keys) -> (keys -> List Log.Output) -> (keys -> Task () ()) -> Program
program decoder forge logging app = do
  (Ok keys) <-
    Settings.decode decoder
      |> Task.andThen forge
      |> Task.exit
  let outputs = logging keys
  Task.perform outputs (app keys)
    |> Prelude.fmap (\_ -> ())

