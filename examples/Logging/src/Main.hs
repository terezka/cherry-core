module Main where

import qualified Cherry.Program as Program
import qualified Cherry.Text as Text
import qualified Cherry.Task as Task
import qualified Cherry.List as List
import qualified Cherry.Dict as Dict
import qualified Cherry.Program as Program
import qualified Cherry.Settings as Settings
import qualified Cherry.Log as Log
import qualified Cherry.Terminal as T
import qualified Control.Concurrent
import qualified Control.Exception.Safe as Exception
import qualified Prelude
import qualified GHC.Stack as Stack
import qualified Network.HTTP as HTTP
import Cherry.Basics
import Cherry.Log
import Cherry.Program (Program)
import Cherry.Text (Text)
import Cherry.Task (Task)
import Cherry.List (List)
import Cherry.Dict (Dict)
import Cherry.Maybe (Maybe(..))
import Cherry.Result (Result(..))


main :: Program
main =
  Program.program decoder init outputs app



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
    context "dying" [] <| do
      context "more" [] <| do
        good "> hello 5"
        Task.enter (Exception.throwString "noooo!!")
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


outputs :: Keys -> List Output
outputs keys =
  [ bugsnag keys
  , terminal message
  , file "log.txt" compact
  ]


bugsnag :: Keys -> Output
bugsnag keys =
  let open =
        Task.succeed ()

      write _ (Entry _ _ _ _ _ context) = do
        Control.Concurrent.threadDelay 5000000
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
            T.write "Production!\n"

      close _ =
        Task.succeed ()
  in
  custom open write close



-- SETTINGS


{-| -}
data Settings =
  Settings
    { sEnvironment :: Environment
    , sPort :: Int
    }


{-| -}
decoder :: Settings.Decoder Settings
decoder =
  Settings.succeed Settings
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

