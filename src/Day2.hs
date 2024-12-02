{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day2
    (
    day2
   ,day2b
   ,_input
    )
    where

day2 :: String -> Int
day2 input = length $ filter id $ map safe $ parseInput input

day2b :: String -> Int
day2b input = length $ filter id $ map safeb $ parseInput input

safe :: (Ord a, Num a) => [a] -> Bool
safe xs = (\ys -> all (\y-> y<=3 && y >=1) ys || all (\y-> y <= -1 && y>= -3) ys)  $ zipWith (-) xs (tail xs)

safeb :: (Ord a, Num a) => [a] -> Bool
safeb xs = safe xs || any safe oneRemoved
  where oneRemoved = [ deleteAt a xs | a <- [0..(length xs -1)]]

deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = lft ++ rgt
  where (lft, _:rgt) = splitAt idx xs

_input :: String
_input="7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"

parseInput :: String -> [[Int]]
parseInput input = map (map read . words) (lines input)