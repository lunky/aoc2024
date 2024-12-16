{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day5
    (
    day5
   ,day5b
   ,_input
    )
    where
import Data.List.Split ( splitOn )
import Data.List ( elemIndex, foldl' )
import Data.Maybe ( fromJust )

day5 :: String -> Int
day5 input = sum $ map center $ filter valid pagesToProduce
    where (pageOrderingRules,pagesToProduce) = parseInput input
          valid update = all (`validRule` update) pageOrderingRules

center :: [Int] -> Int
center xs = (!!) xs (length xs `div` 2)

day5b :: String -> Int
day5b input =   sum  $
                map center $
                go $ map fixUpdate $
                filter (not.valid) pagesToProduce
    where
        go xs = if not (all valid xs) 
                       then go $ map fixUpdate  xs
                       else xs
        fixUpdate update = foldl' fixOrder update $ invalidRules update
        invalidRules update = filter (\y-> not $ validRule y update) pageOrderingRules
        valid update = all (`validRule` update) pageOrderingRules
        (pageOrderingRules,pagesToProduce) = parseInput input


validRule :: Eq a => (a, a) -> [a] -> Bool
validRule (lhs,rhs) update
    = notElem lhs update || notElem rhs update
                         || fromJust (elemIndex lhs update)
                          < fromJust (elemIndex rhs update)

fixOrder :: Eq a => [a] -> (a, a) -> [a]
fixOrder update (lhs,rhs) = swap lhsIndex rhsIndex update
    where
        Just lhsIndex = elemIndex lhs update
        Just rhsIndex = elemIndex rhs update

swap :: Int -> Int -> [a] -> [a]
swap f s xs = map snd . foldr (\x a ->
        if fst x == f then ys !! f : ys !! s : a
        else if fst x == s then a
        else x : a) [] $ ys
    where ys = zip [0..] xs

parseInput :: String -> ([(Int,Int)], [[Int]])
parseInput input = (pageOrderingRules,pagesToProduce)
    where [top,bottom] = splitOn "\n\n" input
          pageOrderingRules = map (\y-> (read $ take 2 y, read $ drop 3 y)) $ lines top
          pagesToProduce = map (map read . splitOn ",") $ lines bottom

_input :: String
_input="47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13\n\n75,47,61,53,29\n97,61,53,29,13\n75,29,13\n75,97,47,61,53\n61,13,29\n97,13,75,29,47"