module Internal.Queue (Queue, init, push, close, listen) where

import qualified Control.Exception.Safe as Exception
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified List
import Control.Concurrent.Chan (Chan)
import Control.Concurrent.MVar (MVar)
import Control.Monad (void)
import Control.Concurrent (forkIO)
import Prelude (IO, return)
import Internal.Entry (Entry)
import Basics
import Maybe (Maybe (..))
import Result (Result (..))
import Dict (Dict)
import List (List)
import Array (Array)
import Set (Set)
import Char (Char)


{-| -}
data Queue
  = Queue (Chan Message) (MVar ())


data Message
  = NewEntry Entry
  | Done


{-| -}
init :: IO Queue
init = do
  channel <- Chan.newChan
  lock <- MVar.newEmptyMVar
  return (Queue channel lock)


{-| -}
push :: Entry -> Queue -> IO ()
push entry (Queue channel lock) =
  Chan.writeChan channel (NewEntry entry)


{-| -}
close :: Queue -> IO ()
close (Queue channel lock) = do
  Chan.writeChan channel Done
  MVar.readMVar lock


{-| -}
listen :: Queue -> (Entry -> IO ()) -> IO () -> IO ()
listen (Queue channel lock) write close =
  let loop = do
        msg <- Chan.readChan channel
        case msg of
          NewEntry entry -> do
            Exception.tryAny (write entry)
            loop

          Done -> do
            close
            MVar.putMVar lock ()
  in
  void (forkIO loop)

