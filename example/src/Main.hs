module Main where

import qualified String
import qualified Task
import qualified Debug
import qualified Interop
import Cherry.Prelude


main =
  Task.attempt application


application :: Task x ()
application = do
  flags <- Environment.decode
  Server.listen 9000 <| \request -> do
    bugsnag <- Reference.new (Bugsnag.init request [])
    let key = Key.key flags bugsnag
    let route =
          Route.find request
            [ Route.map Book.byId (s "api" </> s "book" </> int)
            , Route.map Book.all  (s "api" </> s "book" </> s "all")
            ]
    route key


-- BOOK


byId :: App.Key -> Request -> Id -> Http.Response Book
byId key request id = do
  book <- query key "SELECT * FROM books WHERE id = 2"




-- POSTGRES


query :: App.Key -> Sql -> Task x a
query key sql =
  App.trace key "SQL" sql (DB.get sql)



-- KEY


data Key =
  Key
    { settings :: Settings
    , bugsnag :: Reference Bugsnag.Event
    }


data Trace =
  Trace
    { group : String
    , start : Time.Posix
    , end : Time.Posix
    , context : Json.Value
    }


request :: Settings -> String -> Json.Value -> (Key -> Task x a) -> Task x a
request settings title details toRequest = do
  bugsnag <- Reference.init (Bugsnag.init "Request" title details [])
  toRequest (Key settings bugsnag)


trace :: Key -> String -> Json.Value -> Task x a -> Task x a
trace key group context task = do
  started <- Time.now
  result <- Task.onError (Task.succeed Err) (Task.map Ok task)
  ended <- Time.now
  let new = Trace group started ended context
  Reference.modify (bugsnag key) (Bugsnag.add new)
  case result of
    Ok ok -> Task.succeed ok
    Err err -> do
      Bugsnag.send err (bugsnag key)
      Task.fail err

