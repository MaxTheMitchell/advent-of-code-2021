import qualified Data.Set as Set 
import Data.Maybe ( mapMaybe )
import Data.List ( sort ) 

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

type Pos = (Int, Int)
type Grid = [[Int]]

silverBody :: String -> String 
silverBody str = 
  let 
      grid@(row:_) = parse str
  in
  show . sum $ map (`risk` grid) (makePoints [0..(length row - 1)] [0..(length grid - 1)])

goldBody :: String -> String
goldBody str = 
  let 
      grid@(row:_) = parse str
  in
    show 
    . product 
    . take 3 
    . reverse 
    . sort 
    $ mapMaybe 
      (`basinSize` grid) 
      (makePoints [0..(length row - 1)] [0..(length grid - 1)])


basinSize :: Pos -> Grid -> Maybe Int
basinSize pos grid = 
    case risk pos grid of
      0 -> Nothing 
      _ -> Just . Set.size $ getBasin pos grid

getBasin :: Pos -> Grid -> Set.Set Pos
getBasin pos grid = 
  let 
      Just p =  atGrid pos grid 
  in 
    foldl Set.union (Set.singleton pos)
    . map (`getBasin` grid)
    $ filter (\point -> case atGrid point grid of 
      Nothing -> False
      Just v  -> v > p && v /= 9 
    )(adjs pos)

risk :: Pos -> Grid -> Int
risk pos grid =
  let 
      Just p =  atGrid pos grid 
  in 
    case filter (p >=) $ mapMaybe (`atGrid` grid) (adjs pos) of 
        [] -> p + 1
        _ -> 0

adjs :: Pos -> [Pos]
adjs  (x, y)= zip (repeat x) [y-1, y+1] ++ zip [x-1, x+1] (repeat y)

makePoints :: [Int] -> [Int] -> [Pos]
makePoints xs ys = 
  concatMap (\x -> map (\y -> (x, y)) ys) xs

atGrid :: Pos -> Grid -> Maybe Int
atGrid (x, y) grid@(row:_)
  | (x < 0 || x >= length row) || (y < 0 || y >= length grid) = Nothing
  | otherwise = Just ((grid !! y) !! x)  
atGrid _ _ = Nothing 

parse :: String -> Grid
parse = map (map (\c -> read [c])). lines
