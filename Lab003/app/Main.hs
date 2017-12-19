module Main where

import Data.List (foldl', foldl1)
import Data.Eq (Eq)
import Control.Monad (forM_)
import System.IO (hFlush, stdout)

import qualified FruitParser as P
import qualified CheckoutSystem as S

import Fruit
import CheckoutSystem


showOffer :: CheckoutSystem -> IO ()
showOffer sys =
    case offer sys of   OfferOne -> do
                            putStrLn "buy one, get one free on Apples"

                        OfferTwo -> do
                            putStrLn "3 for the price of 2 on Oranges"
                        
                        otherwise -> do
                            return ()


main :: IO ()
main = do
    putStrLn $ "apple: " ++ (show (fruitPrice Apple))
    putStrLn $ "orange: " ++ (show (fruitPrice Orange))
    putStrLn ""

    let syss = [  CheckoutSystem Nil
                , CheckoutSystem OfferOne
                , CheckoutSystem OfferTwo ]
    
    putStrLn "e.g. Orange, Apple, Apple, Orange, Orange"

    forM_ syss loop
        
    where
        loop sys = do
            showOffer sys
            
            putStr "> "

            hFlush stdout
            str <- getLine

            let ret = P.parseFruits str

            case ret of Right r0 -> success sys r0
                        Left r1 -> fail sys r1

        success sys r0 = do
            let info = S.checkout sys r0
            putStrLn $ "fruits: " ++ ((show . _fruits) info)
            putStrLn $ "cost: " ++ ((show . _cost) info)
            putStrLn ""
        
        fail sys r1 = do
            putStrLn $ show r1
            putStrLn ""
