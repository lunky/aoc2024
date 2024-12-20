{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day7
    ( 
    day7
   ,day7b
   ,_input
    )
    where
import Data.List.Split (splitOn)
import Data.List (foldl')

data Sign = Plus | Multiply | Concat deriving (Eq,Ord,Show,Read)

day7 :: String -> Int 
day7 input = sum $ map fst $ filter day7' $ parseInput input
  where
    day7' :: (Int, [Int]) -> Bool
    day7' (check, xs) = elem check $ day7'' xs
    day7'' (x:xs) = map (foldl' (\acc (y, z)-> case z of 
                           Plus -> y + acc 
                           Multiply -> y * acc)  x . zip xs ) operations
      where operations = generateCombinations (length xs) [Plus,Multiply]

numDigits :: Int -> Integer
numDigits 0 = 0
numDigits n = 1 + numDigits (div n 10) 

day7b :: String -> Int
day7b input = sum $ map fst $ filter day7b' $ parseInput input
  where 
    day7b' :: (Int, [Int]) -> Bool
    day7b' (check, xs) = elem check $ day7b'' xs
    day7b'' (x:xs) = map (foldl' (\acc (y, z) -> case z of 
                           Plus -> y + acc 
                           Concat -> acc * 10 ^ numDigits y + y
                           Multiply -> y * acc)  x . zip xs ) operations

      where 
        operations = generateCombinations (length xs) [Plus,Multiply,Concat]

parseInput :: String -> [(Int, [Int])]
parseInput input =  map ((\[lhs,rhs]->(read lhs, map read $ splitOn " " rhs)) . splitOn ": ") ( lines input)

_input :: String
_input="190: 10 19\n3267: 81 40 27\n83: 17 5\n156: 15 6\n7290: 6 8 6 15\n161011: 16 10 13\n192: 17 8 14\n21037: 9 7 18 13\n292: 11 6 16 20"

generateCombinations :: Int -> [Sign] -> [[Sign]]
generateCombinations 0 _ = [[]]
generateCombinations n signs = [x:xs | x <- signs, xs <- generateCombinations (n-1) signs]
