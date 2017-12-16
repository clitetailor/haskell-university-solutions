module QuickSort where

import System.Environment (getArgs)
import Data.List (foldl')

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

addStr :: String -> Int -> String
addStr accum num =
    accum ++ (show num) ++ " "

toInt :: String -> Int
toInt i0 = read i0

main :: IO ()
main = do
    list <- getArgs
    let sortedList = quicksort (fmap toInt list)
    let str = foldl' addStr "" sortedList
    putStr str
