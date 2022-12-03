module Advent.Day02 where

instr :: (Int, Int) -> (Char, Int) -> (Int, Int)
instr (x, y) (d, n)
    | d == 'u' = (x, y - n)
    | d == 'd' = (x, y + n)
    | d == 'f' = (x + n, y)
    | otherwise = (0, 0)

instr' :: (Int, Int, Int) -> (Char, Int) -> (Int, Int, Int)
instr' (x, y, a) (d, n)
    | d == 'u' = (x, y, a - n)
    | d == 'd' = (x, y, a + n)
    | d == 'f' = (x + n, y + (n * a), a)
    | otherwise = (0, 0, 0)

main :: IO ()
main = do
    input <- lines <$> readFile "inputs/2021/02.txt"

    let xs = (\[x, y] -> (head x, read y)) . words <$> input

    let (x, y) = foldl instr (0, 0) xs
    let (x', y', _) = foldl instr' (0, 0, 0) xs

    print [x * y, x' * y'] -- [1692075, 1749524700]
