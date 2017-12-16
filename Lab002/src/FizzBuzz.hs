module FizzBuzz (
  fizzBuzz,
  main
) where

import System.IO (hFlush, stdout)
import Control.Monad (forM_)
import Data.List (foldl')

toInt :: String -> Int
toInt str = read str

fizzBuzz :: Int -> [String]
fizzBuzz n0 = do
    foldl' addToList [] [1..n0]
    where
        addToList l0 i0
            | i0 `mod` 6 == 0 = l0 ++ ["FizzBuzz"]
            | i0 `mod` 2 == 0 = l0 ++ ["Fizz"]
            | i0 `mod` 3 == 0 = l0 ++ ["Buzz"]
            | otherwise = l0 ++ [show i0]

main :: IO ()
main = do
    putStr "n = "
    hFlush stdout
    str <- getLine
    let n = toInt str
    if (n >= 1)
        then do
            let list = fizzBuzz n
            forM_ list $ \item -> do
                putStr (item ++ " ")
        else do
            putStrLn "\nn should larger than or equal 1"
