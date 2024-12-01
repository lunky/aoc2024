{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE TupleSections #-}
module Day1
    (
    day1
   ,day1b
   ,_input
    )
    where

import Data.List ( group, sort, transpose, foldl' )
import Data.List.Split ( splitOn )
import qualified Data.Map.Strict as Map

day1 :: String -> Int
day1 input = sum $ map (\(a,b)->abs ( a-b)) $ (\[a,b]->zip a b) $ map sort $ parseInput input

day1b :: String -> Int
day1b input = sum $ map (\x-> x * Map.findWithDefault 0 x mapb) lista
    where mapb = freq listb
          [lista, listb] = parseInput input

_input :: String
_input="3   4\n4   3\n2   5\n1   3\n3   9\n3   3"

parseInput :: String -> [[Int]]
parseInput input =  transpose $ map (map (\y-> read y::Int) . splitOn "   ") (lines input)


freq :: (Foldable t, Ord a, Num a) => t a -> Map.Map a a
freq = foldl' (\acc x -> Map.insertWith (\_ b->b+1) x 1 acc) Map.empty