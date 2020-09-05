module RefExtras.Memo
  ( Memo
  , accessMemo
  , freezeMemo
  , newMemo
  , thawMemo
  , tryReadMemo
  , readMemo
  ) where

import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Prelude
import RefExtras.EVar (EVar, accessEVar, newEventualEVar, newReadyEVar, readEVar, tryReadEVar)
import RefExtras.XVar (XVar, newXVar, readXVar, splitXVar)

-- | A 'Memo' lets us cache the results of computations by key,
-- ensuring that we compute only as necessary and in order of access.
-- All operations are guaranteed not to lock the structure while computing
-- or waiting for results.
newtype Memo k a = Memo { unMemo :: XVar (HashMap k (EVar a)) }

-- | Creates a new empty 'Memo'.
newMemo :: MonadIO m => m (Memo k a)
newMemo = fmap Memo (newXVar HashMap.empty)

-- | You can /access/ an 'Memo' by telling it how to compute the value!
-- If there is already a value, it simply returns it instead of computing it.
-- If the computation fails, the exception propagates, leaving that 'Memo' cell empty.
-- Blocks on var and action.
accessMemo :: (MonadUnliftIO m, Eq k, Hashable k) => Memo k a -> k -> m a -> m a
accessMemo (Memo v) k act = splitXVar v $ \m write ->
  case HashMap.lookup k m of
    Just w -> write m *> accessEVar w act
    Nothing -> newEventualEVar (\w -> write (HashMap.insert k w m)) act

-- | Reads the memoized value if present and ready now. Non-blocking.
tryReadMemo :: (MonadIO m, Eq k, Hashable k) => Memo k a -> k -> m (Maybe a)
tryReadMemo (Memo v) k = do
  m <- readXVar v
  case HashMap.lookup k m of
    Nothing -> pure Nothing
    Just w -> tryReadEVar w

-- | Reads the memoized value if present, blocking on computations.
readMemo :: (MonadIO m, Eq k, Hashable k) => Memo k a -> k -> m (Maybe a)
readMemo (Memo v) k = do
  m <- readXVar v
  case HashMap.lookup k m of
    Nothing -> pure Nothing
    Just w -> readEVar w

bindFor :: (Monad t, Traversable t, Applicative f) => t a -> (a -> f (t b)) -> f (t b)
bindFor t f = fmap join (traverse f t)

-- | Freeze the 'Memo' with all values ready now. Non-blocking.
freezeMemo :: (MonadIO m, Eq k, Hashable k) => Memo k a -> m (HashMap k a)
freezeMemo (Memo v) = do
  m <- readXVar v
  fmap HashMap.fromList $ bindFor (HashMap.toList m) $ \(k, w) -> do
    ma <- tryReadEVar w
    case ma of
      Just a -> pure [(k, a)]
      _ -> pure []

-- | Thaw a 'HashMap' into a new 'Memo'.
thawMemo :: MonadIO m => HashMap k a -> m (Memo k a)
thawMemo m = do
  n <- traverse newReadyEVar m
  v <- newXVar n
  pure (Memo v)
