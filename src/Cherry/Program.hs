{-# LANGUAGE RankNTypes #-}

module Cherry.Program (Program, program) where

import qualified Data.Text
import qualified System.Environment
import qualified Cherry.Internal.Settings as Settings
import qualified Cherry.Task as Task
import qualified Cherry.Dict as Dict
import qualified Cherry.Log as Log
import qualified Prelude
import Cherry.Text (Text)
import Cherry.Task (Task)
import Cherry.List (List)
import Cherry.Dict (Dict)
import Cherry.Result (Result(..))
import Cherry.Basics


{-| The type which `main` must always return.

  > main :: Program
  > main =
  >   Program.program decoder init targets app

-}
type Program =
  Prelude.IO ()


{-| Make a program. Arguments:

      1. A decoder for environment variables. See Settings.hs
      2. A function which, provided your environment variables, makes a set of resources which should only be initiated once and is used through out the app.
      3. A function which, provided your environment variables, determines the targets for your log entries. See Log.hs to learn more about logging.
      4. Your application.

-}
program :: forall settings keys. Settings.Decoder settings -> (settings -> Task Text keys) -> (keys -> List Log.Target) -> (keys -> Task () ()) -> Program
program decoder forge logging app = do
  (Ok keys) <-
    Settings.run decoder
      |> Task.andThen forge
      |> Task.exit
  let outputs = logging keys
  Task.perform outputs (app keys)
    |> Prelude.fmap (\_ -> ())

