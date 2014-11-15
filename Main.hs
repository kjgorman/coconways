module Main where

import Control.Comonad

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
