-- | Classes that generalize operations on mutable references.
module RefExtras.Classes where

import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.ST (ST)
import Control.Monad.STM (STM)
import Data.STRef (STRef, readSTRef, writeSTRef)
import LittleRIO (SomeRef, readSomeRef, writeSomeRef)
import UnliftIO.IORef (IORef, atomicModifyIORef', modifyIORef', readIORef, writeIORef)

-- | A reference that we can read and write, but not necessarily without
-- contending with intervening mutations.
class ReadWriteRef r m where
  readRef :: r a -> m a
  writeRef :: r a -> a -> m ()

-- | /Unsafe/ in the sense that in most cases we can implement modify by
-- reading then writing, but we're not guaranteed that no writes have occurred in
-- the meantime. This is true for IORefs, which is why we need a separate core method.
-- It is not true for STM, which means this is actually safe for TVars.
unsafeModifyRef :: (Monad m, ReadWriteRef r m) => r a -> (a -> a) -> m ()
unsafeModifyRef ref f = do
  a <- readRef ref
  let a' = f a
  writeRef ref $! a'

-- | See notes on safety for 'unsafeModifyRef'.
unsafeAtomicModifyRef :: (Monad m, ReadWriteRef r m) => r a -> (a -> (a, b)) -> m b
unsafeAtomicModifyRef ref f = do
  a <- readRef ref
  let (a', b) = f a
  writeRef ref $! a'
  pure $! b

-- | A reference that offers the ability to mutate with a pure function.
-- However, we might not be able to observe the result without intervening mutations.
class ReadWriteRef r m => ModifyRef r m where
  modifyRef :: r a -> (a -> a) -> m ()

-- | A reference that we can mutate *and observe* with a pure function.
class ModifyRef r m => AtomicRef r m where
  atomicModifyRef :: r a -> (a -> (a, b)) -> m b

-- | Atomically swap a value in a reference.
swapRef :: AtomicRef r m => r a -> a -> m a
swapRef r a = atomicModifyRef r (\z -> (a, z))

instance MonadIO m => ReadWriteRef IORef m where
  readRef = readIORef
  writeRef = writeIORef

instance MonadIO m => ModifyRef IORef m where
  modifyRef = modifyIORef'

instance MonadIO m => AtomicRef IORef m where
  atomicModifyRef = atomicModifyIORef'

instance ReadWriteRef (STRef s) (ST s) where
  readRef = readSTRef
  writeRef = writeSTRef

instance ReadWriteRef TVar STM where
  readRef = readTVar
  writeRef = writeTVar

-- These /unsafe/ impls are safe for TVars because STM guarantees that
-- the vars don't change between reading and writing.

instance ModifyRef TVar STM where
  modifyRef = unsafeModifyRef

instance AtomicRef TVar STM where
  atomicModifyRef = unsafeAtomicModifyRef

instance MonadIO m => ReadWriteRef SomeRef m where
  readRef = readSomeRef
  writeRef = writeSomeRef
