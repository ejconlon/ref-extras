module RefExtras.UVar
  ( UVar
  , newUVar
  , takeUVar
  , isTakenUVar
  , cloneUVar
  ) where

import Control.Monad.IO.Class (MonadIO)
import UnliftIO.MVar (MVar, isEmptyMVar, newEmptyMVar, newMVar, tryTakeMVar)

-- | A "unique var" - something that can only be taken once.
newtype UVar a = UVar { unUVar :: MVar a }

newUVar :: MonadIO m => a -> m (UVar a)
newUVar = fmap UVar . newMVar

takeUVar :: MonadIO m => UVar a -> m (Maybe a)
takeUVar = tryTakeMVar . unUVar

isTakenUVar :: MonadIO m => UVar a -> m Bool
isTakenUVar = isEmptyMVar . unUVar

-- | Create a new 'UVar' with the contents of this.
-- If this is taken, the created 'UVar' is empty.
-- Otherwise, takes this and creates the other.
-- In all cases, this is left taken.
cloneUVar :: MonadIO m => UVar a -> m (UVar a)
cloneUVar u = takeUVar u >>= maybe (fmap UVar newEmptyMVar) newUVar
