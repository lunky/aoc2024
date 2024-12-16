{-# LANGUAGE TupleSections #-}
module Day6
    (
    day6
   ,day6b
   ,_input
    )
    where
import Prelude hiding (Left,Right)
import Data.List (nub)
import Data.Set (Set)
import qualified Data.Set as Set

data Direction = Up | Down | Left | Right deriving (Show,Eq,Ord)

type Coord = (Int,Int)
type Grid = [String]
type Loc = (Coord,Direction)
type Path = [Loc]

day6 :: String -> Int
day6 input = length $ nub $ day6' input

day6' :: String -> [Coord]
day6' input = map fst $ tick grid
  where grid = parseInput input


day6b :: String -> Int
day6b input = length $ day6b' input

day6b' :: String -> [(Coord, Bool)]
day6b' input =  filter snd $ map (\o-> (o,isLoop o grid)) realPath
  where realPath = nub $ removeStart $ day6' input
        grid = parseInput input
        removeStart = init

parseInput :: String -> [String]
parseInput = lines

get :: Coord -> Grid -> Maybe Char
get (x,y) grid = if x < width && x >= 0 && y < height && y >= 0
                 then Just $ grid !! y !! x
                 else Nothing
    where
        width = length grid
        height = if width > 0 then length $ head grid else 0


startLocation :: Grid -> Loc
startLocation grid = (start ,startDirection)
  where start = head $ startCoords (=='^') grid
        startDirection = direction $ case get start grid of
                                            Just loc -> loc
                                            _ -> error "can't find start"

tick :: Grid -> Path
tick grid = go [startLocation grid]
  where
        go [] = []
        go (loc:path) =  
          case get (nextMove xy dir) grid of
            Nothing  -> loc:path
            Just '#' -> go $ (nextMove xy (turn dir),turn dir) : loc : path
            _ -> go $ (nextMove xy dir,dir) : loc : path
            where (xy,dir) = loc

isLoop :: Coord -> Grid -> Bool
isLoop obstacle grid = go Set.empty $ startLocation grid
  where
        get' :: Coord -> Maybe Char
        get' coord = if coord==obstacle then Just '#' else get coord grid
        go :: Set Loc ->  Loc -> Bool
        go seen loc = Set.member loc seen  || -- loop detected 
                      case get' (nextMove xy dir) of
                        Nothing  -> False -- out of bounds
                        Just '#' -> go seen (xy, turn dir) -- obstacle, turn
                        _ -> go (Set.insert loc seen) (nextMove xy dir, dir) -- keep going
            where (xy,dir) = loc

nextMove :: Coord -> Direction -> Coord
nextMove (x,y) Up  = (x,y-1)
nextMove (x,y) Down = (x,y+1)
nextMove (x,y) Left = (x-1,y)
nextMove (x,y) Right = (x+1,y)

direction :: Char -> Direction
direction c = case c of
            '^' -> Up
            'v' -> Down
            '<' -> Left
            '>' -> Right
            _ -> error "invalid direction"

turn :: Direction -> Direction
turn Up = Right
turn Right = Down
turn Down = Left
turn Left = Up

startCoords :: (Char -> Bool) -> [String] -> [(Int, Int)]
startCoords predicate =
  concatMap (\(y, r) -> map ((,y) . fst) $ filter (predicate . snd) $ zip [0..] r) . zip [0..]

_input :: String
_input="....#.....\n.........#\n..........\n..#.......\n.......#..\n..........\n.#..^.....\n........#.\n#.........\n......#..."

_grid :: [String]
_grid = parseInput _input