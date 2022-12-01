{-# LANGUAGE LambdaCase #-}

import Control.Comonad
import Data.Functor
import Data.Map (findWithDefault, fromList)
import Data.Matrix hiding (fromList)
import Data.Maybe

import Parser

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
  (Live, n) | n == 2 || n == 3 -> Live
  (Dead, 3)-> Live
  _ -> Dead

stabilize :: Grid -> [Grid]
stabilize g = if g == g2 then return g else g2 : stabilize g2 where
  g2 = peek (Store (1, 1) g =>> evolve)

-- Parsing

instance Read Cell where
  readsPrec _ "o" = [(Live, "")]
  readsPrec _ "b" = [(Dead, "")]

lookups :: Rle -> (Int, Int) -> Cell
lookups rle xy = findWithDefault Dead xy (fromList flat)  where
  raw = fmap (concat . fmap (\case (r, c) -> replicate r $ read [c])) rle
  ind = zip [0..] $ fmap (zip [0..]) raw
  flat = concat $ fmap (\(x, rw) -> fmap (\(y, cell) -> ((8+x, 8+y), cell)) rw) ind

-- IO

main :: IO ()
main = void $ traverse print $ take 500 $ stabilize $ board where
  rle = fromMaybe [] $ parseRle $ 
    "24bo11b$22bobo11b$12b2o6b2o12b2o$11bo3bo4b2o12b2o$2o8bo5bo3b2o14b$" ++
      "2o8bo3bob2o4bobo11b$10bo5bo7bo11b$11bo3bo20b$12b2o!"
  board = matrix 60 60 $ lookups rle

