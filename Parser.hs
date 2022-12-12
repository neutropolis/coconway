module Parser (parseRle) where

import Rle

import Data.Either ( fromRight )
import Data.Maybe
import Text.ParserCombinators.Parsec

-- TODO there is a clear pattern that I am rewriting again and again,
-- some kind of withDelimiter (and optional end). It would simplify
-- the code a lot.

rle :: GenParser Char st Rle
rle = do meta <- meta
         header <- header
         items <- body
         char '!'
         end <- many anyChar
         eof
         return $ Rle meta header items end

meta :: GenParser Char st [Meta]
meta = do h <- line
          t <- remainingLine
          return $ h : t

line :: GenParser Char st Meta
line = do char '#'
          C <$> (oneOf "Cc" >> space >> many (noneOf "\n")) <|>
            N <$> (char 'N' >> space >> many (noneOf "\n")) <|>
            O <$> (char 'O' >> space >> many (noneOf "\n")) <|>
            p

p :: GenParser Char st Meta
p = do oneOf "PR" <* many space
       x <- read <$> many1 digit <* many space
       y <- read <$> many1 digit
       return $ P (abs x) (abs y)

remainingLine :: GenParser Char st [Meta]
remainingLine = newline >> (meta <|> return [])

header :: GenParser Char st [Header]
header = do h <- property
            t <- remainingProperty
            return $ h : t

property :: GenParser Char st Header
property =  spaces >>
            ((X <$> numProp "x") <|> 
             (Y <$> numProp "y") <|> 
             (Rule <$> strProp "rule"))

lhs :: String -> GenParser Char st ()
lhs s = string s >> spaces >> char '=' >> spaces

numProp :: String -> GenParser Char st Int
numProp s = lhs s >> read <$> many1 digit

strProp :: String -> GenParser Char st String
strProp s = lhs s >> many (noneOf "\n")

remainingProperty :: GenParser Char st [Header]
remainingProperty = (char ',' >> header) <|> (newline >> return [])

body :: GenParser Char st [[Item]]
body = do h <- many item
          t <- remaining
          return $ h : t

remaining :: GenParser Char st [[Item]]
remaining = (char '$' >> body) <|> return []

item :: GenParser Char st Item
item = do rc <- runCount
          t  <- tag
          return (rc, t)

runCount :: GenParser Char st RunCount
runCount = (read <$> many1 digit) <|> return 1

tag :: GenParser Char st Tag
tag = char 'b' <|> char 'o' <|> (letter >> return 'b')

parseRle :: String -> Rle
parseRle s = fromRight empty (parse rle "(unknown)" s) where
  empty = Rle [] [X 50, Y 80] [] ""

