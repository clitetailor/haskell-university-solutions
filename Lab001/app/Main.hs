module Main where

import System.IO (hFlush, stdout)

main :: IO ()
main = do
    putStrLn "What is your name?"
    putStr "> "
    hFlush stdout
    str <- getLine
    putStrLn ""
    if (str == "exit")
        then do
            putStrLn "Goodbye!"
        else do
            putStrLn ("Hello " ++ str ++ ". Nice to meet you " ++ str ++ "!\n")
            main
