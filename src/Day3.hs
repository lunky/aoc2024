{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day3
    ( 
    day3
   ,day3b
   ,_input
   ,_input2
    )
    where
import qualified  Text.Parsec as Parsec
import Text.ParserCombinators.Parsec ((<|>))
import qualified Text.ParserCombinators.Parsec as P
import Data.List (foldl')
import Data.Either 

data Instruction = Mul Int | Do | Dont  deriving (Show)
data MulState = MulDo | MulDont deriving (Show)
data State = DoOrDont MulState !Int deriving (Show)
type Parser = Parsec.Parsec String ()

day3 :: String -> Int 
day3 input = sum $ map (\(Mul x)->x) $ filter mul $ parseInstructions input
  where mul (Mul _) = True
        mul _ = False
day3b :: String -> Int
day3b input = readInstructions $ parseInstructions input

parseInstructions :: String -> [Instruction]
parseInstructions input = fromRight [] $ parse parseManyMuls input

readInstructions :: [Instruction] -> Int
readInstructions = (\(DoOrDont _ result) -> result) . foldl' go (DoOrDont MulDo 0)
  where
    go (DoOrDont MulDo acc) (Mul val) = DoOrDont MulDo (acc + val)
    go (DoOrDont MulDont acc) (Mul _) = DoOrDont MulDont acc
    go (DoOrDont _ acc) Do = DoOrDont MulDo acc
    go (DoOrDont _ acc) Dont = DoOrDont MulDont acc

parse :: Parser a -> String -> Either Parsec.ParseError a
parse p = P.parse p ""

parseMul:: Parser Instruction
parseMul= do
  _ <- P.string "mul("  
  num1 <- P.many1 P.digit
  _ <- P.char ','
  num2 <- P.many1 P.digit
  _ <- P.string ")"  
  return $  Mul (read num1 * read num2)

parseManyMuls :: Parser [Instruction]
parseManyMuls = P.many loop
  where
    loop = P.try parseInstruction <|> P.try (P.anyChar >> loop)        
    parseInstruction = P.try parseDo P.<|> P.try parseDont P.<|> P.try parseMul

parseDo :: Parser Instruction
parseDo = do 
  _ <- P.string "do()"
  return Do

parseDont :: Parser Instruction
parseDont = do
    _ <- P.string "don't()"
    return Dont

_input :: String
_input="xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

_input2 :: String
_input2="xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
