import Data.List.Split ( splitOn )
import qualified Data.Map as Map

main :: IO ()
main = silver input >> gold input

test :: IO String
test = readFile "test.txt"

input :: IO String
input = readFile "input.txt"

gold :: IO String -> IO ()
gold = (>>= putStrLn . (++) "gold:\n" . goldBody)

silver :: IO String -> IO () 
silver = (>>= putStrLn . (++) "silver:\n" . silverBody)

---------------------------------------------------------------------
type Point = (Int, Int)

type Line = (Point, Point)

type Grid = Map.Map Point Int 

silverBody :: String -> String 
silverBody = 
  show 
  . length
  . filter (/= 1)
  . Map.elems
  . makeGrid
  . parse

goldBody :: String -> String
goldBody = 
  show 
  . length
  . filter (/= 1)
  . Map.elems
  . makeGridGold
  . parse

placeLines :: [Line] -> [Point]
placeLines = concatMap makePoints 

makeGrid :: [Line] -> Grid
makeGrid = foldl addToGrid Map.empty

addToGrid :: Grid -> Line -> Grid
addToGrid grid l = foldl insertGrid grid $ makePoints l 

makeGridGold :: [Line] -> Grid
makeGridGold = foldl addToGridGold Map.empty

addToGridGold :: Grid -> Line -> Grid
addToGridGold grid l = foldl insertGrid grid $ makePointsGold l 

makePoints :: Line -> [Point]
makePoints ((x1, y1), (x2, y2))
  | x1 == x2 = map (\y -> (x1, y)) [min y1 y2..(max y1 y2)]
  | y1 == y2 = map (\x -> (x, y1)) [min x1 x2..(max x1 x2)]
  | otherwise = []

makePointsGold :: Line -> [Point]
makePointsGold ((x1, y1), (x2, y2))
  | x1 == x2 = map (\y -> (x1, y)) [min y1 y2..(max y1 y2)]
  | y1 == y2 = map (\x -> (x, y1)) [min x1 x2..(max x1 x2)]
  | otherwise = zip (myRange x1 x2) (myRange y1 y2)

myRange :: Int -> Int -> [Int] 
myRange a b 
  | a <= b = [a..b]
  | otherwise = reverse [b..a]

insertGrid :: Grid -> Point -> Grid 
insertGrid grid p = 
  let 
    newVal = case Map.lookup p grid of 
      Just v -> v + 1
      Nothing -> 1
  in 
    Map.insert p newVal grid  

parse :: String -> [Line]
parse = map 
  (
    (\[s, e] -> (s, e))
    . map ((\[x, y] -> (x, y)). map read . splitOn ",")
    . splitOn " -> "
  ) . lines 