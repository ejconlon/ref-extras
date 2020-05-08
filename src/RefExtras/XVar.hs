module RefExtras.XVar
  ( XVar
  , newXVar
  , readXVar
  , writeXVar
  , modifyXVar
  , atomicModifyXVar
  , modifyXVarM
  , lockXVarM
  , atomicModifyXVarM
  , splitXVar
  ) where

import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import RefExtras.Classes (AtomicRef (..), ModifyRef (..), ReadWriteRef (..))
import UnliftIO.Exception (finally)
import UnliftIO.MVar (MVar, modifyMVar, modifyMVar_, newMVar, putMVar, readMVar, swapMVar, takeMVar, withMVar)

newtype XVar a = XVar { unXVar :: MVar a } deriving (Eq)

newXVar :: MonadIO m => a -> m (XVar a)
newXVar = fmap XVar . newMVar

readXVar :: MonadIO m => XVar a -> m a
readXVar = readMVar . unXVar

writeXVar :: MonadIO m => XVar a -> a -> m ()
writeXVar (XVar m) = void . swapMVar m

modifyXVar :: MonadIO m => XVar a -> (a -> a) -> m ()
modifyXVar (XVar m) f = liftIO (modifyMVar_ m (pure . f))

atomicModifyXVar :: MonadIO m => XVar a -> (a -> (a, b)) -> m b
atomicModifyXVar (XVar m) f = liftIO (modifyMVar m (pure . f))

modifyXVarM :: MonadUnliftIO m => XVar a -> (a -> m a) -> m ()
modifyXVarM = modifyMVar_ . unXVar

lockXVarM :: MonadUnliftIO m => XVar a -> (a -> m b) -> m b
lockXVarM = withMVar . unXVar

atomicModifyXVarM :: MonadUnliftIO m => XVar a -> (a -> m (a, b)) -> m b
atomicModifyXVarM = modifyMVar . unXVar

-- Locks the XVar and runs a function with the current value and a write callback.
-- The XVar is unlocked when the function completes or the *first* time the
-- write callback is invoked. Subsequent calls overwrite the XVar, but may
-- be interleaved with other writes. If the write callback is not invoked at
-- all, the original value is restored.
splitXVar :: MonadUnliftIO m => XVar a -> (a -> (a -> m ()) -> m b) -> m b
splitXVar (XVar m) f = do
  a <- takeMVar m
  i <- newMVar True
  let write force x = modifyMVar_ i $ \p -> do
        if p
          then putMVar m x
          else when force (void (swapMVar m x))
        pure False
  finally (f a (write True)) (write False a)

instance MonadIO m => ReadWriteRef XVar m where
  readRef = readXVar
  writeRef = writeXVar

instance MonadIO m => ModifyRef XVar m where
  modifyRef = modifyXVar

instance MonadIO m => AtomicRef XVar m where
  atomicModifyRef = atomicModifyXVar
