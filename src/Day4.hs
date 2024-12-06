{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day4
      (
    day4
   ,day4b
   ,_input
    )
    where

import Data.List.Split ( chunksOf )
import Data.Map (Map)
import qualified Data.Map as Map

day4 :: String -> Int
day4 input = length $ concatMap (\y->vectorAdjacent y 'M' m) $ xs m
  where m = parseInput input

day4b :: String -> Int
day4b input =
  length $ filter id $ map (((`elem` xmases) . map (\y -> Map.findWithDefault '.' y parsed)) . xshape) (getAs parsed)
  where parsed = parseInput input
        xmases = ["MSMS","SSMM","SMSM","MMSS"]


_input :: String
_input="MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX"

parseInput :: String -> Map (Int, Int) Char
parseInput input = Map.fromList
                    $ concatMap (\(y,xs')->zipWith (\ x z -> ((x, y), z)) [1..] xs')
                    $ zip [1..]
                    $ lines input


getAdjacent :: (Ord a1, Ord b, Num a1, Num b, Eq a2) => (a1, b) -> a2 -> Map (a1, b) a2 -> [(a1, b)]
getAdjacent (x,y) l m = filter (\z-> Map.lookup z m == Just l) $ adjacent (x,y)

vectorAdjacent :: (Ord a, Ord b, Num b, Num a) => (a, b) -> Char -> Map (a, b) Char -> [String]
vectorAdjacent (x,y) l m = filter (=="XMAS")
                                  $ map (\(x1,y1) -> map (\z->Map.findWithDefault '.' z m) $ vector (x,y) (x1,y1) )
                                  $ getAdjacent (x,y) l m

vector :: (Num b, Num a, Eq b, Eq a) => (a, b) -> (a, b) -> [(a, b)]
vector (x,y) (x1,y1) = [(x,y), (x1,y1),(x2,y2), (x3,y3)]
  where
    x2 = case x-x1 of
      -1 -> x1 + 1
      0  -> x
      1  -> x1 - 1
      _ -> error "invalid"
    y2 = case y-y1 of
      -1 -> y1 + 1
      0  -> y1
      1  -> y1 - 1
      _ -> error "invalid"
    x3 = case x-x1 of
      -1 -> x2 + 1
      0  -> x2
      1  -> x2 - 1
      _ -> error "invalid"
    y3 = case y-y1 of
      -1 -> y2 + 1
      0  -> y2
      1  -> y2 - 1
      _ -> error "invalid"

xs :: Map (Int,Int) Char -> [(Int,Int)]
xs m = Map.keys $ Map.filter (=='X') m

getAs :: Map (Int,Int) Char -> [(Int,Int)]
getAs m = Map.keys $ Map.filter (=='A') m

adjacent :: (Num a, Num b) => (a, b) -> [(a, b)]
adjacent (x,y) = [
        (x-1,y-1), (x,y-1), (x+1,y-1),
        (x-1,y),            (x+1, y),
        (x-1,y+1), (x,y+1), (x+1,y+1) ]

xshape :: (Num a, Num b) => (a, b) -> [(a, b)]
xshape (x,y) = [ (x-1,y-1), (x+1,y-1), (x-1,y+1), (x+1,y+1) ]

_showGrid :: Map (Int, Int) Char -> IO ()
_showGrid m = mapM_ putStrLn
              $ chunksOf maxX
              $ [Map.findWithDefault '.' (x,y) m | y <- [1..maxY], x <- [1..maxX]]
  where [_, _, maxX, maxY] = extents $ map fst $ Map.toList m
        extents [(x,y)] = [x,y,x,y]
        extents (xy:xys) = foldr (\(x',y') [minX', minY', maxX', maxY']
                      ->  [min x' minX', min y' minY', max x' maxX', max y' maxY']) (extents [xy]) xys