module Main where

import Lib

import System.IO (hFlush, stdout)
import Control.Monad (forM_, when)
import Data.Function

toInt :: String -> Int
toInt str = read str

fizzBuzz :: Int -> IO ()
fizzBuzz n0 = do
    forM_ [1..n0] $ \i -> do
        when (i `mod` 2 == 0) $ putStr "Fizz"
        when (i `mod` 3 == 0) $ putStr "Buzz"
        when ((i `mod` 2 /= 0) && (i `mod` 3 /= 0)) $ putStr (show i)
        putStr " "
        hFlush stdout

main :: IO ()
main = do
    putStr "n = "
    hFlush stdout
    str <- getLine
    let n = toInt str
    if (n >= 1)
        then do
            fizzBuzz n
        else do
            putStrLn "\nn should larger than or equal 1"
