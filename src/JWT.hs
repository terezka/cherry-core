module JWT (init, Env, expirationSecs, jwks) where

import Cherry.Prelude
import qualified Data.ByteString as ByteString
import qualified Jose.Jwk
import qualified Json.Decode as D
import qualified Interop
import qualified Task
import qualified Environment
import qualified Result
import qualified String
import qualified Maybe
import qualified File
import qualified Data.Aeson as Aeson


{-| -}
data Env = Env
  { expirationSecs :: Int
  , jwks :: List Jose.Jwk.Jwk
  }


{-| -}
init :: Task String Env
init =
  Task.map2 Env
    acquireExpirationSecs
    acquireJwks


acquireJwks :: Task String (List Jose.Jwk.Jwk)
acquireJwks = do
  path <- Environment.optional "JWK_PATH"
  contents <- File.read (Maybe.withDefault "secrets/jwk.sig" path)
  case decode (String.toByteString contents) of
    Ok jwks -> Task.succeed jwks
    Err err -> Task.fail err


decode :: ByteString.ByteString -> Result.Result String (List Jose.Jwk.Jwk)
decode file =
  Aeson.eitherDecodeStrict file
    |> Result.fromEither
    |> Result.mapError (\msg -> "Could not decode JWK file: " ++ String.fromList msg)


acquireExpirationSecs :: Task String Int
acquireExpirationSecs = do
  string <- Environment.optional "JWT_EXPIRATION_SECS"
  let secs = Maybe.withDefault defaultExpirationSecs (Maybe.andThen String.toInt string)
  Task.succeed secs


defaultExpirationSecs :: Int
defaultExpirationSecs =
  2 * 60 * 60

