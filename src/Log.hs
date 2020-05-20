module Log
  ( -- * Entries
    Entry.Entry(..), Entry.Severity(..)

    -- * Targets
  , Task.Target, Task.terminal, Task.file, Task.custom

    -- * Formatting
  , Entry.pretty, Entry.compact, Entry.json

    -- * Sending an entry
  , Task.debug, Task.info, Task.warning, Task.error, Task.alert, Task.exception, Task.segment

    -- * Adding extra context
  , Entry.value, Entry.int, Entry.float, Entry.text, Entry.lookup

    -- * Tracer
  , Task.Tracer, Task.tracer
  ) where

{-|

Module      : Log
Description : Helpers for logging.
License     : BSD 3
Maintainer  : terezasokol@gmail.com
Stability   : experimental
Portability : POSIX

-}

import qualified List
import qualified GHC.Stack as Stack
import qualified Internal.Entry as Entry
import qualified Internal.Queue as Queue
import qualified Internal.Task as Task
import qualified Control.Exception.Safe as Control
import qualified Json.Encode as Json
import Control.Monad (void)
import Internal.Entry (Entry)
import Internal.Task (Task)
import Cherry.Prelude
import Prelude (IO, return, sequence_)

