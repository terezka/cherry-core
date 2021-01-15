{-# LANGUAGE PackageImports #-}

module Token (Token, generate, resolve) where


import qualified Prelude as P
import qualified Data.Either as Either
import qualified Data.Maybe as HMaybe
import qualified Data.Aeson.Types as Json
import qualified Data.Aeson as Aeson
import qualified "text" Data.Text as T
import qualified "text" Data.Text.Lazy as TL
import qualified "text" Data.Text.Encoding as Encoding
import qualified "text" Data.Text.Encoding.Error as Encoding
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Data.Time.Clock as Clock
import qualified Data.Maybe as HMaybe
import qualified Jose.Jwt as Jwt
import qualified Jose.Jwa as Jwa
import qualified Control.Monad.Except as Except
import qualified Text.Read
import qualified Internal.Shortcut as Shortcut
import qualified Interop
import qualified JWT
import qualified Result
import qualified Task
import qualified Internal.Task
import qualified Maybe
import qualified String
import qualified Time
import Cherry.Prelude


{-| -}
type Token =
  String



-- GENERATE


{-| -}
generate :: JWT.Env -> String -> Task String Token
generate env userId = do
  now <- Time.now
  payload <- encodeJwt env (claims env userId now)
  Task.succeed (String.fromByteString payload)


claims :: JWT.Env -> String -> Time.POSIX -> Jwt.Payload
claims env userId now =
  Jwt.JwtClaims
    { Jwt.jwtIss = HMaybe.Nothing
    , Jwt.jwtSub = HMaybe.Just (T.pack (String.toList userId))
    , Jwt.jwtAud = HMaybe.Nothing
    , Jwt.jwtExp = HMaybe.Just (Jwt.IntDate (expirationTime env now))
    , Jwt.jwtNbf = HMaybe.Nothing
    , Jwt.jwtIat = HMaybe.Nothing
    , Jwt.jwtJti = HMaybe.Nothing
    }
    |> Aeson.encode
    |> BL.toStrict
    |> Jwt.Claims


expirationTime :: JWT.Env -> Time.POSIX -> Time.POSIX
expirationTime env now =
  now + P.fromIntegral (JWT.expirationSecs env)


encodeJwt :: JWT.Env -> Jwt.Payload -> Task String B.ByteString
encodeJwt env payload =
  let fromEither either =
        case either of
          Either.Right (Jwt.Jwt encoded) -> Result.Ok encoded
          Either.Left err -> Result.Err (toErrorMsg err)
  in
  Jwt.encode (JWT.jwks env) (Jwt.JwsEncoding Jwa.RS256) payload
    |> Shortcut.map fromEither
    |> Internal.Task.Task



-- RESOLVE


{-| -}
resolve :: JWT.Env -> Token -> Task String String
resolve env token =
  let check :: Time.POSIX -> B.ByteString -> Task String String
      check now token_ =
        decodeClaims token_
          |> Task.andThen (isExpired now)
          |> Task.andThen parseClaims
  in do
  now <- Time.now
  token_ <- decodeToken env token
  check now token_


decodeToken :: JWT.Env -> Token -> Task String B.ByteString
decodeToken env token =
  let utf8Token = String.toByteString token
      settings = Jwt.JwsEncoding Jwa.RS256
      fromEither jwt =
        case jwt of
          Either.Right ( Jwt.Jws ( header, json ) ) -> Ok json
          Either.Left err -> Err (toErrorMsg err)
  in
  Jwt.decode (JWT.jwks env) (HMaybe.Just settings) utf8Token
    |> Shortcut.map fromEither
    |> Internal.Task.Task


decodeClaims :: B.ByteString -> Task String Jwt.JwtClaims
decodeClaims claims =
  claims
    |> BL.fromStrict
    |> Aeson.eitherDecode
    |> Result.fromEither
    |> Result.mapError String.fromList
    |> Task.fromResult


isExpired :: Time.POSIX -> Jwt.JwtClaims -> Task String Jwt.JwtClaims
isExpired now claims =
  if expiration now claims < now then
    Task.fail "Token is expired."
  else
    Task.succeed claims


expiration :: Time.POSIX -> Jwt.JwtClaims -> Time.POSIX
expiration now claims =
  let jwtExpiration = Maybe.fromHMaybe (Jwt.jwtExp claims)
      (Jwt.IntDate date) = Maybe.withDefault (Jwt.IntDate now) jwtExpiration
  in
  date


parseClaims :: Jwt.JwtClaims -> Task String String
parseClaims claims =
  claims
    |> Jwt.jwtSub
    |> Maybe.fromHMaybe
    |> Maybe.map (String.fromByteString << Encoding.encodeUtf8)
    |> Result.fromMaybe "Could not parse claims."
    |> Task.fromResult



-- SHARED


toErrorMsg :: Jwt.JwtError -> String
toErrorMsg error =
  case error of -- TODO err type ?
    Jwt.KeyError _ -> "No suitable key or wrong key type."
    Jwt.BadAlgorithm _ -> "The supplied algorithm is invalid."
    Jwt.BadDots _ ->  "Wrong number of \".\" characters in the JWT."
    Jwt.BadHeader _ -> "Header couldn't be decoded or contains bad data."
    Jwt.BadClaims -> "Claims part couldn't be decoded or contains bad data."
    Jwt.BadSignature -> "Signature is invalid."
    Jwt.BadCrypto -> "A cryptographic operation failed."
    Jwt.Base64Error _ -> "A base64 decoding error."


