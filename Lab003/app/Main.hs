module Main where

import Lib

data Node = Tree { left :: Node, right :: Node }
        |   Leaf { value :: Int }
        deriving (Show)

main :: IO ()
main = do
    let node = Tree {
        left = Leaf {
            value = 1
        },
        right = Leaf {
            value = 2
        }
    }
    let newNode = increase node
    putStrLn $ show node
    putStrLn $ show newNode
    where
        increase n0 =
            case n0 of  Tree _ _ -> n0 { left = increase $ left n0 }
                        Leaf _ -> n0 { value = (value n0) + 1 }
