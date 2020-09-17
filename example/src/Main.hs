module Main where

import qualified String
import qualified Task
import qualified Debug
import qualified Interop
import Cherry.Prelude


main =
  Task.attempt (Settings "the connection") application


application :: Task Settings x ()
application = do
  book <-
    Task.mapKey Settings.postgres <| do
      query "SELECT * FROM books WHERE id = 2"


data Settings =
  Settings
    { postgres :: Postgres.Settings
    , bugsnag :: Bugsnag.Event
    }


-- POSTGRESS


query :: SQL -> Task Postgres.Settings x String
query sql = do
  Task.mapKey (trace sql) <| do



trace :: SQL -> Bugsnag.Event -> Bugsnag.Event
trace sql event =
  event { traces = Trace sql now now : traces event }