module Logging (outputs) where

import qualified Keys
import qualified Network.HTTP as HTTP
import qualified Cherry.Task as Task
import qualified Cherry.Terminal as T
import qualified Cherry.Dict as Dict
import qualified Cherry.List as List
import Cherry.Basics
import Cherry.Log
import Cherry.List (List(..))


outputs :: Keys.Keys -> List Output
outputs keys =
  [ bugsnag
  , terminal message
  , file "log.txt" compact
  ]


bugsnag :: Output
bugsnag =
  let open =
        Task.succeed ()

      write _ (Entry _ _ _ _ _ context) = do
        HTTP.simpleHTTP (HTTP.getRequest "http://hackage.haskell.org/")
          |> Task.enter
          |> Task.andThen (print context)

      print context _ =
        case Dict.get "user" context of
          Just "tereza" -> T.write "tereza"
          _ -> T.write "no user"

      close _ =
        Task.succeed ()
  in
  custom open write close
