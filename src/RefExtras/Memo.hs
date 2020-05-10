module RefExtras.Memo
  ( Memo
  , accessMemo
  , clearMemo
  , freezeMemo
  , newMemo
  , thawMemo
  , tryReadMemo
  , writeMemo
  ) where

import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Prelude
import RefExtras.EVar (EVar, accessEVar, newEventualEVar, newReadyEVar, tryReadEVar, writeEVar)
import RefExtras.XVar (XVar, newXVar, readXVar, splitXVar, writeXVar)

newtype Memo k a = Memo { unMemo :: XVar (HashMap k (EVar a)) }

newMemo :: MonadIO m => m (Memo k a)
newMemo = fmap Memo (newXVar HashMap.empty)

clearMemo :: MonadIO m => Memo k a -> m ()
clearMemo (Memo v) = writeXVar v HashMap.empty

accessMemo :: (MonadUnliftIO m, Eq k, Hashable k) => Memo k a -> k -> m a -> m a
accessMemo (Memo v) k act = splitXVar v $ \m write ->
  case HashMap.lookup k m of
    Just w -> write m >> accessEVar w act
    Nothing -> newEventualEVar (\w -> write (HashMap.insert k w m)) act

tryReadMemo :: (MonadIO m, Eq k, Hashable k) => Memo k a -> k -> m (Maybe a)
tryReadMemo (Memo v) k = do
  m <- readXVar v
  case HashMap.lookup k m of
    Nothing -> pure Nothing
    Just w -> tryReadEVar w

-- Blocks if k is being populated
writeMemo :: (MonadUnliftIO m, Eq k, Hashable k) => Memo k a -> k -> a -> m ()
writeMemo (Memo v) k a = splitXVar v $ \m write ->
  case HashMap.lookup k m of
    Just w -> write m >> writeEVar w a
    Nothing -> do
      w <- newReadyEVar a
      write (HashMap.insert k w m)

bindFor :: (Monad t, Traversable t, Applicative f) => t a -> (a -> f (t b)) -> f (t b)
bindFor t f = fmap join (traverse f t)

freezeMemo :: (MonadIO m, Eq k, Hashable k) => Memo k a -> m (HashMap k a)
freezeMemo (Memo v) = do
  m <- readXVar v
  fmap HashMap.fromList $ bindFor (HashMap.toList m) $ \(k, w) -> do
    ma <- tryReadEVar w
    case ma of
      Just a -> pure [(k, a)]
      _ -> pure []

thawMemo :: MonadIO m => HashMap k a -> m (Memo k a)
thawMemo m = do
  n <- traverse newReadyEVar m
  v <- newXVar n
  pure (Memo v)
