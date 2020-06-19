
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
    -- * Tracer
    Task.Tracer, Task.tracerless, Task.tracer

    -- * Targets
  , Task.Target, Task.terminal, Task.file, Task.target

    -- * Formatting
  , Entry.pretty, Entry.compact, Entry.json

    -- * Entry
  , Entry.Entry(..), Entry.Severity(..), Entry.lookup

  ) where

import qualified Internal.Entry as Entry
import qualified Internal.Task as Task


