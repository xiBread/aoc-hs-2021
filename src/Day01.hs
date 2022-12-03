module Advent.Day01 where

import Data.List (tails)

windows :: [Int] -> [[Int]]
windows xs = zipWith (const id) (drop 2 xs) $ take 3 <$> tails xs

incrs :: [Int] -> Int
incrs xs = sum [1 | (y, x) <- zip xs $ tail xs, x > y]

main :: IO ()
main = do
    input <- lines <$> readFile "inputs/01.txt"

    let xs = read <$> input
    let ys = map sum $ windows xs

    print $ incrs <$> [xs, ys] -- [1583, 1627]
