module Control.Comonad where

import Control.Applicative ((<$>))

data U x = U [x] x [x]
           deriving Show

uright (U a b (c:cs)) = U (b:a) c cs
uleft  (U (a:as) b c) = U as a (b:c)

instance Functor U where
  fmap f (U l c r) = U (map f l) (f c) (map f r)

class Functor w => Comonad w where
  (=>>)    :: w a -> (w a -> b) -> w b
  coreturn :: w a -> a
  cojoin   :: w a -> w (w a)
  x =>> f = fmap f (cojoin x)

instance Comonad U where
  cojoin a = move uleft uright a
  coreturn (U _ b _ ) = b

move :: (a -> a) -> (a -> a) -> a -> U a
move a b u = U (tail $ iterate a u) u (tail $ iterate b u)

toList :: U a -> Int -> [a]
toList (U ls c rs) n = reverse (take n ls) ++ [c] ++ take n rs

data Plane a = Plane (U (U a))

instance Functor Plane where
  fmap f (Plane u) = Plane ((f <$>) <$> u)

instance Comonad Plane where
  cojoin   p         = Plane (rows <$> cols p)
  coreturn (Plane u) = coreturn . coreturn $ u

above, below, left, right :: Plane a -> Plane a
above (Plane u) = Plane (uleft u)
below (Plane u) = Plane (uright u)
left  (Plane u) = Plane (uleft <$> u)
right (Plane u) = Plane (uright <$> u)

rows, cols :: Plane a -> U (Plane a)
rows = move left right
cols = move above below
