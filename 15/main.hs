import Data.Maybe (fromJust)
import Data.Map as Map((!), Map, keys, fromList, member, toList, union) 
import Data.Graph.AStar ( aStar )
import Data.HashSet as HashSet (HashSet, fromList)

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
type Grid = Map.Map Point Int


silverBody :: String -> String 
silverBody = show . risk . parse

goldBody :: String -> String
goldBody = show . risk . timesFive . parse  

findPath :: Grid -> [Point]
findPath grid = 
  let
    max = gridMax grid 
  in 
    fromJust $ aStar 
        (neihbors grid) 
        (\_ k -> grid ! k) 
        (\(x, y) -> (fst max - x) + (snd max - y)) 
        (== max) 
        (0,0)

risk :: Grid -> Int
risk grid = 
  sum 
  . map (grid !) 
  $ findPath grid  

neihbors :: Grid -> Point -> HashSet Point
neihbors grid (x, y) = 
  HashSet.fromList
  $ filter (`Map.member` grid) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

parse :: String -> Grid
parse =
   Map.fromList 
   . concatMap (\(y, s) -> 
      map (\(x, c) -> 
        ((x, y), read[c])) (zip [0..] s)) 
   . zip [0..] 
   . lines 

timesFive :: Grid -> Grid
timesFive grid = foldl1 Map.union $ map (moveGrid grid) [(x, y) | x<-[0..4], y<-[0..4]] 

gridMax :: Grid -> Point
gridMax = maximum . Map.keys

moveGrid :: Grid -> Point -> Grid
moveGrid grid (0, 0) = grid 
moveGrid grid (moveX, moveY) = 
  let 
    (maxX, maxY) = gridMax grid
  in 
    Map.fromList 
    . map (\((x, y), i) -> 
      ((x + moveX + maxX * moveX, y + moveY + maxY * moveY),
        ((i + moveX + moveY - 1) `mod` 9) + 1
      ))
    $ Map.toList grid 