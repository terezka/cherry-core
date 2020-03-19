module Settings (Settings(..), decoder) where

import qualified Cherry.Settings as S
import qualified Cherry.Text as Text
import qualified Prelude
import Cherry.Text (Text)
import Cherry.Basics


{-| -}
data Settings =
  Settings
    { environment :: Text
    , port :: Int
    }


{-| -}
decoder :: S.Decoder Settings
decoder =
  S.succeed Settings
    |> S.optional "ENVIRONMENT" S.text "DEVELOPMENT"
    |> S.optional "PORT" S.int 9000