module RefExtras.EVar
  ( EVar
  , accessEVar
  , newEventualEVar
  , newReadyEVar
  , readEVar
  , tryReadEVar
  , writeEVar
  , modifyEVar
  ) where

import Control.Monad (join, void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import UnliftIO.Exception (finally)
import UnliftIO.IORef (atomicWriteIORef, newIORef, readIORef)
import UnliftIO.MVar (MVar, modifyMVar, newEmptyMVar, newMVar, putMVar, readMVar, swapMVar, tryReadMVar)

newtype EVar a = EVar { unEVar :: MVar (Maybe a) }

-- Blocks on var and action
accessEVar :: MonadUnliftIO m => EVar a -> m a -> m a
accessEVar (EVar w) act = modifyMVar w (fmap (\a -> (Just a, a)) . maybe act pure)

-- Blocks on action
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

-- Non-blocking
newReadyEVar :: MonadIO m => a -> m (EVar a)
newReadyEVar = fmap EVar . newMVar . Just

-- Non-blocking
tryReadEVar :: MonadIO m => EVar a -> m (Maybe a)
tryReadEVar = fmap join . tryReadMVar . unEVar

-- Blocks on var
readEVar :: MonadIO m => EVar a -> m (Maybe a)
readEVar = readMVar . unEVar

-- Blocks on var
writeEVar :: MonadIO m => EVar a -> a -> m ()
writeEVar (EVar w) = void . swapMVar w . Just

-- Blocks on var and action
modifyEVar :: MonadUnliftIO m => EVar a -> m a -> (a -> (a, b)) -> m b
modifyEVar (EVar w) act f = modifyMVar w (fmap (\a -> let (a', b) = f a in (Just a', b)) . maybe act pure)
