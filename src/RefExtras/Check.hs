module RefExtras.Check
  ( CheckEffect (..)
  , checkEffectRef
  , checkEffectXVar
  , runCheckEffectRef
  , runCheckEffectXVar
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import RefExtras.Classes (AtomicRef, atomicModifyRef, readRef)
import RefExtras.XVar (XVar, atomicModifyXVarM)

-- Conditionally updates an AtomicRef. First reads the value with the prepare function, which
-- chooses to write with the commit function or chooses to return. This function allows you to
-- sequence a read, some effectful operation, and an optional write, all with the caveat that
-- the ref may have changed between the read and the write.
checkEffectRef :: (MonadIO m, AtomicRef r m) => r a -> (a -> m (Either x b)) -> (x -> a -> (a, b)) -> m b
checkEffectRef ref prepare commit = do
  a <- readRef ref
  e <- prepare a
  case e of
    Left b -> atomicModifyRef ref (commit b)
    Right c -> pure c

-- checkEffectRef but locking around the effectful prepare function. You will block for the duration
-- of the prepare and commit functions but are guaranteed that the var does not change in the
-- meantime.
checkEffectXVar :: MonadUnliftIO m => XVar a -> (a -> m (Either x b)) -> (x -> a -> (a, b)) -> m b
checkEffectXVar ref prepare commit = atomicModifyXVarM ref $ \a -> do
  e <- prepare a
  case e of
    Left b -> pure (commit b a)
    Right c -> pure (a, c)

-- The two prepare and commit functions packaged up.
data CheckEffect m a b where
  CheckEffect :: !(a -> m (Either x b)) -> !(x -> a -> (a, b)) -> CheckEffect m a b

runCheckEffectRef :: (MonadIO m, AtomicRef r m) => r a -> CheckEffect m a b -> m b
runCheckEffectRef ref (CheckEffect prepare commit) = checkEffectRef ref prepare commit

runCheckEffectXVar :: MonadUnliftIO m => XVar a -> CheckEffect m a b -> m b
runCheckEffectXVar ref (CheckEffect prepare commit) = checkEffectXVar ref prepare commit
