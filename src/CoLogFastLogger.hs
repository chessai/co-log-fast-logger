module CoLogFastLogger
  ( withFastLogAction

    -- * Re-exports
    -- ** From co-log
  , module CologCore

    -- ** From fast-logger
  , module SystemLogFastLogger
  ) where

import System.Log.FastLogger as SystemLogFastLogger
import Colog.Core as CologCore
import Prelude hiding (log)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Control.Monad.Catch as Exceptions

type FastLogAction m = LogAction m LogStr

withFastLogAction :: (MonadIO m, Exceptions.MonadMask m)
  => LogType
  -> (FastLogAction m -> m a)
  -> m a
withFastLogAction typ f = Exceptions.bracket
  (liftIO (newFastLogger typ))
  (liftIO . snd)
  (f . (\g -> LogAction (liftIO . g)) . fst)
{-# specialise withFastLogAction :: LogType -> (FastLogAction IO -> IO a) -> IO a #-}
