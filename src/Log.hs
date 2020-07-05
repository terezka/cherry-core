
{-|

Module      : Entry
Description : Work with a logging entry.
License     : BSD 3
Maintainer  : terezasokol@gmail.com
Stability   : experimental
Portability : POSIX

-}

module Log
  ( -- * Customization
    Task.Config

  -- * Default
  , Entry.Basic, Task.basic

    -- * Tracer
  , Task.Tracer, Task.tracerless, Task.customTracer

    -- * Targets
  , Task.Target, Task.terminal, Task.file, Task.target

    -- * Formatting
  , Entry.pretty, Entry.compact, Entry.json

    -- * Entry
  , Entry.Entry(..), Entry.Severity(..)

    -- * Send entries
  , Task.segment
  , Task.debug, Task.info, Task.warning, Task.error, Task.alert, Task.exception

  -- * Adding context
  , Entry.WithMisc(..), Entry.bool, Entry.string, Entry.int, Entry.float, Entry.value
  ) where

import qualified Internal.Entry as Entry
import qualified Internal.Task as Task

