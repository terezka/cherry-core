module Settings (Settings(..), decoder) where

import qualified Cherry.Settings as Settings
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
decoder :: Settings.Decoder Settings
decoder =
  Settings.succeed Settings
    |> Settings.optional "ENVIRONMENT" Settings.text "DEVELOPMENT"
    |> Settings.optional "PORT" Settings.int 9000