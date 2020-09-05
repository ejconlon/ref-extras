module RefExtras.SomeRef
  ( unliftSomeRef
  ) where

import Control.Monad.IO.Unlift (MonadUnliftIO, UnliftIO (..), askUnliftIO)
import LittleRIO (SomeRef (..))
import RefExtras.Classes (ReadWriteRef (..))

-- | We can /demote/ any 'ReadWriteRef' to a 'SomeRef'.
-- ('RIO' uses 'SomeRef' to represent 'State' and 'Writer' references, for example.)
unliftSomeRef :: (MonadUnliftIO m, ReadWriteRef r m) => r a -> m (SomeRef a)
unliftSomeRef ref = do
  UnliftIO run <- askUnliftIO
  pure (SomeRef (run (readRef ref)) (run . writeRef ref))
