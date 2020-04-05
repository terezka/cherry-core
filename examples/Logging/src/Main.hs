module Main where

import qualified Cherry.Program as Program
import qualified Cherry.Text as Text
import qualified Cherry.Task as Task
import qualified Cherry.List as List
import qualified Cherry.Dict as Dict
import qualified Cherry.Settings as Settings
import qualified Cherry.Log as Log
import qualified Cherry.Terminal as T
import qualified Control.Concurrent
import qualified Control.Exception.Safe as Exception
import qualified GHC.Stack as Stack
import qualified Network.HTTP as HTTP
import Cherry.Basics
import Cherry.Log
import Cherry.Prelude


main :: Program
main =
  Program.program decoder init targets app



-- APP


app :: Keys -> Task () ()
app keys =
  context "app" [ ( "online", "true" ) ] <| do
    good "> hello 1"
    debug "print" "Beginning the printing." [ ( "user", "tereza" ), ( "email", "terezasokol@gmail.com" ) ]
    good "> hello 2"
    bad "> hello 3"
    good "> hello 4"
    info "other" "Something." []
    debug "namespace" "Last one." [ ( "user", "tereza" ), ( "email", "terezasokol@gmail.com" ) ]


good :: Stack.HasCallStack => Text -> Task () ()
good string =
  context "good" [ ( "is_ok", "true" ) ] <| do
    T.write (T.green ++ string ++ T.reset ++ T.newline)


bad :: Text -> Task () ()
bad string =
  context "bad" [ ( "is_ok", "false" ) ] <| do
    Task.succeed ()
    error "other" "Something." []



-- LOGGING


targets :: Keys -> List Target
targets keys =
  [ bugsnag keys
  , terminal pretty
  , file "log.txt" compact
  ]


bugsnag :: Keys -> Target
bugsnag keys =
  let open =
        Task.succeed ()

      write _ (Entry _ _ _ _ _ context) = do
        Control.Concurrent.threadDelay 1000000
          |> Task.enter
        HTTP.simpleHTTP (HTTP.getRequest "http://hackage.haskell.org/")
          |> Task.enter
          |> Task.andThen (print context)

      print context _ =
        case environment keys of
          Development ->
            case Dict.get "user" context of
              Just "tereza" -> T.write "tereza\n"
              _ -> T.write "no user\n"

          Production ->
            Task.succeed ()

      close _ =
        Task.succeed ()
  in
  custom open write close



-- KEYS


{-| -}
data Keys =
  Keys
    { environment :: Environment
    , port :: Int
    }


{-| -}
init :: Settings -> Task Text Keys
init (Settings env port) =
  Task.succeed (Keys env port)



-- SETTINGS


{-| -}
data Settings =
  Settings Environment Int


{-| -}
decoder :: Settings.Decoder Settings
decoder =
  Settings.decode Settings
    |> Settings.optional "ENVIRONMENT" parseEnvironment Production
    |> Settings.optional "PORT" Settings.int 9000


data Environment
  = Development
  | Production


parseEnvironment :: Settings.Parser Environment
parseEnvironment =
  let toEnv text =
        case Text.toUpper text of
          "DEVELOPMENT" -> Ok Development
          "PRODUCTION" -> Ok Production
          _ -> Err "Invalid environment."
  in
  Settings.text |> Settings.andThen toEnv
