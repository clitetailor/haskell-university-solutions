module Main where

import Data.List (length, foldl', foldl1)
import Data.Eq (Eq)
import Data.Char (toUpper, toLower)
import Control.Monad (forM_)
import System.IO (hFlush, stdout)
import Text.Parsec (ParsecT, (<|>), parse, eof, many, sepBy)
import Text.Parsec.Char (char, noneOf, oneOf, char)
import Text.ParserCombinators.Parsec (GenParser, ParseError)
import Text.Parsec.Token (symbol)


data Fruit = Apple | Orange deriving (Show)

data Info = Info { _fruits :: [Fruit],
                   _cost :: Int } deriving (Show)

data Offer = Nil | OfferOne | OfferTwo
data CheckoutSystem = CheckoutSystem { offer :: Offer }


instance Eq Fruit where
    Apple == Apple = True
    Orange == Orange = True
    _ == _ = False


fruits :: CheckoutSystem -> [Fruit] -> [Fruit]
fruits sys order =
    case offer sys of   Nil -> order
                        OfferOne -> order ++ freeApples
                            where
                                freeApples = [x | x <- order, x == Apple]
                        OfferTwo -> order


totalCost :: CheckoutSystem -> [Fruit] -> Int
totalCost sys fruits =
    let cost = foldl' sumUp 0 [Apple, Orange]
    in cost - discount
    where
        sumUp :: Int -> Fruit -> Int
        sumUp accum fruit = accum + (fruitPrice fruit) * (nFruit fruit)

        nFruit :: Fruit -> Int
        nFruit fruit = length [f1 | f1 <- fruits, f1 == fruit]

        discount :: Int
        discount =
            case offer sys of   OfferTwo -> floor $ fromIntegral (nFruit Orange) * fromIntegral (fruitPrice Orange) / fromIntegral 3
                                _ -> 0 


checkout :: CheckoutSystem -> [Fruit] -> Info
checkout sys f0 = Info { _fruits = fruits sys f0,
                         _cost = totalCost sys f0 }


fruitPrice :: Fruit -> Int
fruitPrice Apple = 60
fruitPrice Orange = 25

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


    
main :: IO ()
main = do
    putStrLn "e.g. Orange, Apple, Apple, Orange, Orange"

    let syss = [  CheckoutSystem Nil
                , CheckoutSystem OfferOne
                , CheckoutSystem OfferTwo ]
    forM_ syss $ \sys -> do
        showOffer sys
        
        putStr "> "

        hFlush stdout
        str <- getLine
        let ret = parseFruits str
        case ret of     Right r0 -> do
                            let info = checkout sys r0
                            putStrLn $ show info ++ "\n"
                        Left r1 -> putStrLn $ show r1
        
        where
            showOffer sys =
                case offer sys of   OfferOne -> do
                                        putStrLn "buy one, get one free on Apples"
                                    OfferTwo -> do
                                        putStrLn "3 for the price of 2 on Oranges"
                                    otherwise -> do
                                        return ()
