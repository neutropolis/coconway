
module Conway (Cell (Live, Dead), peek, pos, step, Store (Store), Rle) where

import Control.Comonad
import Data.Functor
import Data.Map (findWithDefault, fromList)
import Data.Matrix hiding (fromList)
import Data.Maybe

import Rle

-- Generic comonadic stuff

data Store a = Store { pos :: (Int, Int), peek :: Matrix a } deriving Eq

instance Functor Store where
  fmap f (Store pos m) = Store pos $ fmap f m

instance Comonad Store where
  extract (Store (x, y) m) = getElem x y m
  duplicate (Store pos m) = Store pos $ mapPos (\pos _ -> Store pos m) m

-- Conway's data structures

data Cell = Live | Dead deriving Eq

-- Step Logic

neighbours :: Store Cell -> [Cell]
neighbours (Store (x, y) m) = fmap (\(x, y) -> getElem x y m) pos where
  pos = tail [(x, y) | x <- [x, x-1, x+1], 0 < x, x <= nrows m,
                       y <- [y, y-1, y+1], 0 < y, y <= ncols m]

evolve :: Store Cell -> Cell
evolve s = case (extract s, length $ filter (== Live) $ neighbours s) of
  (Live, n) | n == 2 || n == 3 -> Live
  (Dead, 3)-> Live
  _ -> Dead

step :: Store Cell -> Store Cell
step = (=>> evolve)

