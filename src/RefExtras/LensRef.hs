module RefExtras.LensRef
  ( LensRef
  , mkLensRef
  , wholeLensRef
  , zoomLensRef
  , readLensRef
  , writeLensRef
  , modifyLensRef
  , atomicModifyLensRef
  ) where

import Lens.Micro (Lens', over, set)
import Lens.Micro.Extras (view)
import RefExtras.Classes (AtomicRef (..), ModifyRef (..), ReadWriteRef (..))

-- | A reference /focusing in/ on a part of a larger structure.
data LensRef r a where
  LensRef :: !(r z) -> !(Lens' z a) -> LensRef r a

-- | 'LensRef' constructor
mkLensRef :: r z -> Lens' z a -> LensRef r a
mkLensRef = LensRef

-- | A trivial 'LensRef' that focuses on the whole structure
wholeLensRef :: r a -> LensRef r a
wholeLensRef whole = LensRef whole id

-- | /Zooms/ into a smaller part of the structure
zoomLensRef :: LensRef r a -> Lens' a b -> LensRef r b
zoomLensRef (LensRef whole part) sub = LensRef whole (part . sub)

-- | Read the reference through the lens.
readLensRef :: (Functor m, ReadWriteRef r m) => LensRef r a -> m a
readLensRef (LensRef whole part) = fmap (view part) (readRef whole)

-- | Write the reference through the lens.
-- The 'ModifyRef' constraint is necessary to ensure we read and write
-- the same structure!
writeLensRef :: ModifyRef r m => LensRef r a -> a -> m ()
writeLensRef (LensRef whole part) = modifyRef whole . set part

-- | Modify the reference through the lens.
modifyLensRef :: ModifyRef r m => LensRef r a -> (a -> a) -> m ()
modifyLensRef (LensRef whole part) = modifyRef whole . over part

overWith :: Lens' z a -> (a -> (a, b)) -> z -> (z, b)
overWith l f z =
  let a = view l z
      (a', b) = f a
      z' = set l a' z
  in (z', b)

-- | Atomically modify the reference through the lens.
atomicModifyLensRef :: AtomicRef r m => LensRef r a -> (a -> (a, b)) -> m b
atomicModifyLensRef (LensRef whole part) = atomicModifyRef whole . overWith part

instance (Functor m, ModifyRef r m) => ReadWriteRef (LensRef r) m where
  readRef = readLensRef
  writeRef = writeLensRef

instance (Functor m, ModifyRef r m) => ModifyRef (LensRef r) m where
  modifyRef = modifyLensRef

instance (Functor m, AtomicRef r m) => AtomicRef (LensRef r) m where
  atomicModifyRef = atomicModifyLensRef
