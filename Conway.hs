{-# LANGUAGE LambdaCase #-}

import Control.Comonad
import Data.Functor
import Data.Matrix

-- Generic comonadic stuff

data Store a = Store { pos :: (Int, Int), peek :: Matrix a } deriving Eq

instance Functor Store where
  fmap f (Store pos m) = Store pos $ fmap f m

instance Comonad Store where
  extract (Store (x, y) m) = getElem x y m
  duplicate (Store pos m) = Store pos $ mapPos (\pos _ -> Store pos m) m

-- Conway' data structures

data Cell = Live | Dead deriving Eq
type Grid = Matrix Cell

instance Show Cell where
  show Live = "█"
  show Dead = " "

-- Step Logic

neighbours :: Store Cell -> [Cell]
neighbours (Store (x, y) m) = fmap (\(x, y) -> getElem x y m) pos where
  pos = tail [(x, y) | x <- [x, x-1, x+1], y <- [y, y-1, y+1],
                       0 < x, x <= nrows m, 0 < y, y <= ncols m]

evolve :: Store Cell -> Cell
evolve s = case (extract s, length $ filter (== Live) $ neighbours s) of
  (Live, n) | n < 2 || n > 3 -> Dead
  (Dead, n) | n == 3 -> Live
  (cell, _) -> cell

stabilize :: Grid -> [Grid]
stabilize g = if g == g2 then return g else g2 : stabilize g2 where
  g2 = peek (Store (1, 1) g =>> evolve)

-- IO

main :: IO ()
main = void $ traverse print $ stabilize $ board where
  board = matrix 30 30 (\case
    xy | elem xy glider -> Live -- glider
    otherwise -> Dead) where
    glider = [(1, 2), (2, 3), (3, 1), (3, 2), (3, 3)]

