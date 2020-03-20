module Main where

import qualified Cherry.Program as Program
import qualified App
import qualified Keys
import qualified Settings
import qualified Logging


main :: Program.Program
main =
  Program.program Settings.decoder Keys.init Logging.outputs App.app

