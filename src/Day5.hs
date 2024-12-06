module Day5
    ( 
    day5
   ,day5b
   ,_input
    )
    where
import Data.List.Split
import Data.List
import Data.Maybe
    
day5 :: String -> Int 
day5 input = sum $ map center $ filter valid pagesToProduce
    where (pageOrderingRules,pagesToProduce) = parseInput input
          valid update = all (`validRule` update) pageOrderingRules
          validRule (lhs,rhs) update = notElem lhs update || notElem rhs update 
                                        || fromJust (elemIndex lhs update) < fromJust (elemIndex rhs update)
center :: [Int] -> Int
center xs = (!!) xs $ (length xs `div` 2) 

day5b :: String -> Int
day5b _input = 0

parseInput :: String -> ([(Int,Int)], [[Int]])
parseInput input = (pageOrderingRules,pagesToProduce)
    where [top,bottom] = splitOn "\n\n" input
          pageOrderingRules = map (\y-> (read $ take 2 y, read $ drop 3 y)) $ lines top
          pagesToProduce = map (map read . splitOn ",") $ lines bottom
_input :: String
_input="47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13\n\n75,47,61,53,29\n97,61,53,29,13\n75,29,13\n75,97,47,61,53\n61,13,29\n97,13,75,29,47"