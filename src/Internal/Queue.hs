module Internal.Queue (Queue, init, execute, push) where

import qualified Control.Exception.Safe as Exception
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TBQueue as BQ
import Control.Concurrent.STM.TBQueue (TBQueue)
import Prelude (IO, return, fmap, sequence, sequence_)
import Internal.Entry (Entry)
import Basics
import Maybe (Maybe (..))
import Result (Result (..))
import Text (Text)
import Dict (Dict)
import List (List)
import Array (Array)
import Set (Set)
import Char (Char)


{-| -}
data Queue
  = Queue (TBQueue Message)


data Message
  = NewEntry Entry
  | Done


{-| -}
init :: IO Queue
init = do
  queue <- STM.atomically (BQ.newTBQueue 4096)
  return (Queue queue)


{-| -}
push :: Entry -> Queue -> IO ()
push entry (Queue queue) =
  STM.atomically <| do
    full <- BQ.isFullTBQueue queue
    if not full then
      BQ.writeTBQueue queue (NewEntry entry)
    else
      return ()


{-| -}
execute :: (Entry -> IO a) -> Queue -> IO ()
execute write (Queue queue) =
  let loop = do
        next <- STM.atomically (BQ.readTBQueue queue)
        case next of
          NewEntry entry -> do
            Exception.tryAny (write entry)
            loop

          Done -> do
            return ()
  in do
  STM.atomically (BQ.writeTBQueue queue Done)
  loop

