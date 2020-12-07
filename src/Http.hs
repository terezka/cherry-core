module Http (Request, Response, Key, Decoder(..), getKey, post) where

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Static as Static
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Method as Method
import qualified Network.HTTP.Types.Header as Header
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as TLS
import qualified Internal.Shortcut as Shortcut
import qualified Data.ByteString.Lazy as BL
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
type Request =
  Wai.Request


{-| -}
type Response =
  Wai.Response


data Key
  = Key Client.Manager


{-| -}
getKey :: IO Key
getKey =
  Shortcut.map Key TLS.newTlsManager


data Decoder a
  = Json (D.Decoder a)
  | Text (String -> Result String a)


{-| -}
post :: Key -> String -> E.Value -> Decoder a -> Task String a
post (Key manager) url body decoder =
  Interop.fromResult <| do
    base <- Client.parseRequest (String.toList url)
    let request = base { Client.method = "POST", Client.requestBody = Client.RequestBodyBuilder 1 (E.toBuilder body) }
    response <- Client.httpLbs request manager
    Prelude.return (decodeResponse decoder (Client.responseBody response))


decodeResponse :: Decoder a -> BL.ByteString -> Result String a
decodeResponse decoder bs =
  case decoder of
    Json decoder ->
      D.fromString decoder (String.fromLazyByteString bs)
        |> Result.mapError (\_ -> "Could not decode body.") -- TODO

    Text decoder ->
      decoder (String.fromLazyByteString bs)
