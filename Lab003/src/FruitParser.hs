module FruitParser where

import Text.Parsec (ParsecT, (<|>), parse, eof, many, sepBy)
import Text.Parsec.Char (char, noneOf, oneOf, char)
import Text.ParserCombinators.Parsec (GenParser, ParseError)
import Text.Parsec.Token (symbol)

import Data.Char (toUpper, toLower)
import Data.List (foldl', foldl1)

import Fruit


insensitiveStr str =
    let temp = map trans str in
    let newStr = foldl1 (>>) temp in
    newStr
    where
        trans ch = char (toUpper ch) <|> char (toLower ch)


fruitParser :: GenParser Char st [Fruit]
fruitParser = do
    let p0 = words `sepBy` (many (oneOf ", "))
    chunks <- p0
    return (foldl' (++) [] chunks)
        where
            words = foldl1 (<|>) (map toParser pairs)

            toParser (p0, a1) = do
                p1 <- p0
                return a1

            pairs = [ (insensitiveStr "orange", [Orange])
                    , (insensitiveStr "apple", [Apple])
                    , (noneOf ", ", []) ]


parseFruits :: String -> Either ParseError [Fruit]
parseFruits string =
    parse fruitParser "" string