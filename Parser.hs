module Parser (RunCount, Tag, Item, Rle, parseRle) where

import GHC.Data.Maybe
import Text.ParserCombinators.Parsec

type RunCount = Int
type Tag = Char
type Item = (RunCount, Tag)
type Rle = [[Item]]

rle :: GenParser Char st Rle
rle = do result <- line
         char '!' >> eof
         return result

line :: GenParser Char st Rle
line = do h <- many item
          t <- remaining
          return $ h : t

remaining :: GenParser Char st Rle
remaining = (char '$' >> line) <|> return []

item :: GenParser Char st Item
item = do rc <- runCount
          t  <- tag
          return (rc, t)

runCount :: GenParser Char st RunCount
runCount = (read <$> many1 digit) <|> return 1

tag :: GenParser Char st Tag
tag = char 'b' <|> char 'o'

parseRle :: String -> Maybe Rle
parseRle = rightToMaybe . parse rle "(unknown)"

