import Test.QuickCheck (quickCheck)

import FizzBuzz (fizzBuzz)
import QuickSort (quicksort)

main :: IO ()
main = do
    quickCheck ((fizzBuzz 7) == ["1", "Fizz", "Buzz", "Fizz", "5", "FizzBuzz", "7"])
    quickCheck ((quicksort [8, 4, 16, 20, 1, 24]) == [1, 4, 8, 16, 20, 24])
