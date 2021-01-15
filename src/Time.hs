module Time (POSIX, now) where


import qualified Data.Time.Clock.POSIX as POSIX
import qualified Internal.Task as Task
import qualified Prelude as P
import Cherry.Prelude


type POSIX
  = POSIX.POSIXTime


{-| -}
now :: Task String POSIX
now =
  Task.Task <| do
    time <- POSIX.getPOSIXTime
    P.return (Ok time)
