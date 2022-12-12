{-# LANGUAGE LambdaCase #-}

module Rle where

import Data.List

-- Not sure if C, N, etc. should be hardcoded or more open. The truth is
-- that the definition at conwaylife.com is quite ambiguous so I decided
-- to stick to my own interpretation of it.

data Meta = C String | N String | O String | P Int Int deriving (Show)
data Header = X Int | Y Int | Rule String deriving (Show)
type RunCount = Int
type Tag = Char
type Item = (RunCount, Tag)
data Rle = Rle { _meta   :: [Meta]
               , _header :: [Header]
               , _items  :: [[Item]] 
               , _end    :: String } deriving (Show)

rleName :: Rle -> Maybe String
rleName = fmap (\case (N nme) -> nme) . find (\case (N _) -> True; _ -> False) . _meta

rleComment :: Rle -> Maybe String
rleComment = fmap (\case (C com) -> com) . find (\case (C _) -> True; _ -> False) . _meta
 
rleOwner :: Rle -> Maybe String
rleOwner = fmap (\case (O own) -> own) . find (\case (O _) -> True; _ -> False) . _meta
 
rleOffset :: Rle -> Maybe (Int, Int)
rleOffset = fmap (\case (P x y) -> (x, y)) . find (\case (P _ _) -> True; _ -> False) . _meta

rleX :: Rle -> Maybe Int
rleX = fmap (\case (X x) -> x) . find (\case (X _) -> True; _ -> False) . _header

rleY :: Rle -> Maybe Int
rleY = fmap (\case (Y y) -> y) . find (\case (Y _) -> True; _ -> False) . _header

rleEnd :: Rle -> String
rleEnd = _end

rleUncompress :: Rle -> [String]
rleUncompress = fmap (concatMap $ uncurry replicate) . _items

