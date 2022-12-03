module Advent.Day03 where

import Data.Char (digitToInt)
import Data.List (foldl')

ones :: Int -> [String] -> Int
ones i xs = length $ filter (== '1') $ (!! i) <$> xs

dec :: String -> Int
dec = foldl' (\n d -> n * 2 + digitToInt d) 0

next :: [String] -> Int -> (Char, Char) -> [String]
next n i (x, y) = case n of
    [r] -> [r]
    _ -> filter (\b -> (b !! i) == m) n
  where
    m = if 2 * ones i n >= length n then x else y

ratings :: Int -> [String] -> [String] -> (Int, Int)
ratings i [a] [b] = (dec a, dec b)
ratings i a b = ratings (i + 1) a' b'
  where
    a' = next a i ('1', '0')
    b' = next b i ('0', '1')

main :: IO ()
main = do
    input <- lines <$> readFile "inputs/03.txt"

    let os = [ones i input | i <- [0 .. 11]]

    let gam = (\b -> if 2 * b >= length input then '1' else '0') <$> os
    let eps = (\b -> if b == '1' then '0' else '1') <$> gam

    let (or, cr) = ratings 0 input input

    print [dec gam * dec eps, or * cr] -- [775304, 1370737]
