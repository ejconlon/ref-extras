module RefExtras.EVar
  ( EVar
  , accessEVar
  , newEventualEVar
  , newReadyEVar
  , readEVar
  , tryReadEVar
  ) where

import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import UnliftIO.Exception (finally)
import UnliftIO.IORef (atomicWriteIORef, newIORef, readIORef)
import UnliftIO.MVar (MVar, modifyMVar, newEmptyMVar, newMVar, putMVar, readMVar, tryReadMVar)

-- | The /E/ in 'EVar' stands for /Eventual/.
-- It may or may not have a value, but once it does, it doesn't change.
newtype EVar a = EVar { unEVar :: MVar (Maybe a) }

-- | You can /access/ an 'EVar' by telling it how to compute the value!
-- If there is already a value, it simply returns it instead of computing it.
-- If the computation fails, the exception propagates, leaving the EVar empty.
-- Blocks on var and action.
accessEVar :: MonadUnliftIO m => EVar a -> m a -> m a
accessEVar (EVar w) act = modifyMVar w (fmap (\a -> (Just a, a)) . maybe act pure)

-- | Creates a new 'EVar' with a the given computation.
-- Note that this does not /return/ an 'EVar', but instead creates
-- and shares the 'EVar' before computing it so you can updates references
-- to it first in case of exceptions.
-- Blocks on action.
newEventualEVar :: MonadUnliftIO m => (EVar a -> m ()) -> m a -> m a
newEventualEVar share act = do
  w <- newEmptyMVar
  let e = EVar w
  share e
  -- Use IORef to ensure ONE put in the finally
  i <- newIORef Nothing
  flip finally (readIORef i >>= putMVar w) $ do
    a <- act
    atomicWriteIORef i (Just a)
    pure a

-- | Creates an 'EVar' with an already-computed value.
newReadyEVar :: MonadIO m => a -> m (EVar a)
newReadyEVar = fmap EVar . newMVar . Just

-- | Returns the value if the 'EVar' is computed. Non-blocking.
tryReadEVar :: MonadIO m => EVar a -> m (Maybe a)
tryReadEVar = fmap join . tryReadMVar . unEVar

-- | Returns the value of the 'EVar', blocking on pending computations.
readEVar :: MonadIO m => EVar a -> m (Maybe a)
readEVar = readMVar . unEVar
