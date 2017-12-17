module Main where

import Data.List (length, foldl')
import Data.Eq (Eq)

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


main :: IO ()
main = do
    let sys = CheckoutSystem OfferTwo
    let info = checkout sys [Apple, Orange, Orange, Apple, Orange, Orange]
    putStr $ show info