module Main where

import qualified Cherry.Program as Program
import qualified App
import qualified Keys
import qualified Settings
import qualified Logging
import Cherry.Program (Program)


main :: Program
main =
  Program.program Settings.decoder Keys.init Logging.outputs App.app