{-# LANGUAGE OverloadedStrings #-}

module Http (listen, get, post, Request, Response, text, json, file, body) where

import qualified Control.Exception.Safe as Control
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Maybe as HMaybe
import qualified Data.Either as Either
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding.Error as Encoding
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Data.CaseInsensitive as CI
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Static as Static
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Method as Method
import qualified Network.HTTP.Types.Header as Header
import qualified Prelude
import qualified Maybe
import qualified String
import qualified List
import qualified Tuple
import qualified Result
import qualified Dict
import qualified Task
import qualified Terminal
import qualified Url
import qualified Url.Parser as Parser
import qualified Interop
import qualified Json.Encode as E
import qualified Json.Decode as D
import Cherry.Prelude


{-| -}
type Port =
  Int


{-| -}
type Request =
  Wai.Request


{-| -}
type Response =
  Wai.Response


{-| -}
listen :: Port -> String -> List Route -> Task String ()
listen port public routes =
  let log =
        RequestLogger.logStdoutDev

      static =
        Static.staticPolicy (Static.addBase (String.toList public))

      listen_ =
        Warp.run port application_
          |> Interop.enter
          |> Task.mapError (\_ -> "Could not start server.")

      application_ =
        application public routes
          |> log
          |> static
  in do
  Terminal.write (String.concat [ "Listening on port ", String.fromInt port, "..." ])
  listen_


{-| -}
newtype Route =
  Route (Request -> Url.Url -> Maybe (Task String Response))


{-| -}
get :: Parser.Parser a (Task String Response) -> (Request -> a) -> Route
get parser handler =
  Route <| \request url ->
    if Wai.requestMethod request == Method.methodGet then
      Parser.parse (Parser.map (handler request) parser) url
    else
      Nothing


{-| -}
post :: Parser.Parser a (Task String Response) -> (Request -> a) -> Route
post parser handler =
  Route <| \request url ->
    if Wai.requestMethod request == Method.methodPost then
      Parser.parse (Parser.map (handler request) parser) url
    else
      Nothing


{-| -}
text :: Int -> String -> Response
text statusNo string =
  let status =
        case statusNo of
          200 -> HTTP.status200
          404 -> HTTP.status404
          401 -> HTTP.status401
          501 -> HTTP.status501
          _   -> HTTP.status404 -- TODO
  in
  Wai.responseLBS status [] (String.toLazyByteString string)


{-| -}
json :: Int -> E.Value -> Response
json statusNo value =
  let status =
        case statusNo of
          200 -> HTTP.status200
          404 -> HTTP.status404
          401 -> HTTP.status401
          501 -> HTTP.status501
          _   -> HTTP.status404 -- TODO
  in
  Wai.responseLBS status [] (Builder.toLazyByteString (E.toBuilder value))


{-| -}
file :: Int -> String -> Response
file statusNo path =
  let status =
        case statusNo of
          200 -> HTTP.status200
          404 -> HTTP.status404
          401 -> HTTP.status401
          501 -> HTTP.status501
          _   -> HTTP.status404 -- TODO
  in
  Wai.responseFile status [] (String.toList path) HMaybe.Nothing



-- HELPERS


{-| -}
body :: D.Decoder a -> Request -> Task.Task String a
body decoder request =
  let getChunks :: List B.ByteString -> Task.Task String B.ByteString
      getChunks chunks =
        Wai.requestBody request
          |> Interop.enter
          |> Task.mapError (\_ -> "Body could not be parsed")
          |> Task.andThen (\chunk ->
              if chunk == B.empty
              then Task.succeed (B.concat (List.reverse chunks))
              else getChunks (chunk : chunks)
            )

      decode bs =
        String.fromByteString bs
          |> D.fromString decoder
          |> fromResult

      fromResult result =
        case result of
          Ok v -> Task.succeed v
          Err e -> Task.fail "Body could not be parsed"
  in
  getChunks []
    |> Task.andThen decode


-- INTERNAL


application :: String -> List Route -> Wai.Application
application public routes request respond =
  let url = requestToUrl request
      allRoutes = collectRoutes public routes
  in
  findResponse public url request allRoutes
    |> Task.attempt
    |> Interop.andThen (toSafeResponse >> respond)


requestToUrl :: Request -> Url.Url
requestToUrl request =
  let toPath request =
        Wai.rawPathInfo request
          |> String.fromByteString

      toQuery request =
        Wai.rawQueryString request
          |> String.fromByteString
          |> nothingIfEmpty

      nothingIfEmpty string =
        if String.isEmpty string then
          Nothing
        else
          Just string
  in
  Url.Url
    { Url.path = toPath request
    , Url.query = toQuery request
    }


findResponse :: String -> Url.Url -> Request -> List Route -> Task String Response
findResponse public url request remaining =
  case remaining of
    Route next : rest ->
      case next request url of
        Just response -> response
        Nothing -> findResponse public url request rest

    [] ->
        Task.succeed (file 200 (String.concat [ public, "/404.html" ]))


collectRoutes :: String -> List Route -> List Route
collectRoutes public routes =
  routes ++ [ homeRoute public ]


homeRoute :: String -> Route
homeRoute public =
  get Parser.top <| \_ ->
    Task.succeed (file 200 (String.concat [ public, "/index.html" ]))


toSafeResponse :: Result String Response -> Response
toSafeResponse result =
  case result of
    Result.Ok response -> response
    Result.Err msg -> internalError msg


internalError :: String -> Response
internalError err =
  Wai.responseLBS HTTP.status500 [] (String.toLazyByteString err)


notFound :: Response
notFound =
  Wai.responseLBS HTTP.status404 [] "Route not found"

