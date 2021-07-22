module Instance
  (
  ) where

import Control.Applicative (ZipList(ZipList), getZipList)
import Data.Distributive (distribute)

instance Distributive ZipList where
  -- distribute :: forall f r. Functor f => f (ZipList r) -> ZipList (f r)
  distribute f = ZipList $ takeWhile $ (\i -> ($i) <$> fLookupAble) <$> [0..]
  distribute f = (\i -> (!! i) <$> f) <$> [0..]
      where fLookupAble = (!) <$> f
            (ZipList z) ! idx = let len = length z in if idx < len then Just (z !! idx) else Nothing

