module Main where

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

neighbours :: [Plane a -> Plane a]
neighbours = [left, right, above, below, tl, tr, bl, br]
             where
               tl = left  . above
               tr = right . above
               bl = left  . below
               br = right . below

livingNeighbours :: Plane Bool -> Int
livingNeighbours p = length . filter id $ map (\n -> coreturn $ n p) neighbours

rule :: Plane Bool -> Bool
rule p = case livingNeighbours p of
  2 -> coreturn p
  3 -> True
  _ -> False

step :: Plane Bool -> Plane Bool
step = (=>> rule)

showLine :: U Bool -> String
showLine row = map (\b -> if b then '*' else ' ') $ toList row 6

showPlane :: Plane Bool -> [String]
showPlane (Plane p) = map showLine $ toList p 6

glider :: Plane Bool
glider = Plane $ U (repeat blank) blank rs
       where
           rs = [ line [False, True , False]
                , line [False, False, True ]
                , line [True , True , True ] ] ++ repeat blank
           falses = repeat False
           blank = U (falses) False (falses)
           line l = U (falses) False (l ++ (falses))

main = do
  let plane = glider
  loop plane
  where loop p = do
          _ <- readLn :: IO String
          putStr . unlines . showPlane $ p
          loop (step p)
