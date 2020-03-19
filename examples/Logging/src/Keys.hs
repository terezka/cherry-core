module Keys (Keys, init) where

import qualified Settings
import qualified Cherry.Text as Text
import qualified Cherry.Task as Task
import qualified Prelude
import Cherry.Text (Text)
import Cherry.Task (Task)
import Cherry.Basics
import Settings (Settings(..))


{-| -}
data Keys =
  Keys
    { environment :: Text
    , port :: Int
    }


{-| -}
init :: Settings -> Task Text Keys
init (Settings env port) =
  Task.succeed (Keys env port)