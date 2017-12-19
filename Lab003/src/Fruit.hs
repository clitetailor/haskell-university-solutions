module Fruit where

data Fruit = Apple | Orange deriving (Show)

instance Eq Fruit where
    Apple == Apple = True
    Orange == Orange = True
    _ == _ = False
