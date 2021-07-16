module Pair (
  Pair(..),
  universe,
  (!)
) where

data Pair a where
  Pair :: a -> a -> Pair a
  deriving (Read, Show, Eq)
instance Functor Pair where
  fmap f ~(Pair a1 a2) = Pair (f a1) (f a2)
instance Applicative Pair where
  pure a = Pair a a
  ~(Pair f1 f2) <*> ~(Pair a1 a2) = Pair (f1 a1) (f2 a2)
instance Foldable Pair where
  foldr f b ~(Pair a1 a2) = a1 `f` (a2 `f` b)

(!) :: Pair a -> Bool -> a
~(Pair a1 a2) ! b = if b then a2 else a1

universe :: Pair Bool
universe = Pair False True

