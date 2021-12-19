import Data.List ( find, mapAccumL )
import Data.List.Split ( splitOn )
import qualified Data.Set as Set (Set, fromList, toList, map, intersection, size, union)

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

type Point = (Int, Int, Int)
type Scanner = Set.Set Point
type FullScanner = (Scanner, Point)

silverBody :: String -> String 
silverBody = show . Set.size . foldl1 Set.union . map fst . toFullScanners . parse 

goldBody :: String -> String
goldBody str = 
  let 
    ps = map snd . toFullScanners $ parse str
  in 
    show $ maximum [abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2) | (x1,y1,z1)<-ps, (x2,y2,z2)<-ps]

toFullScanners :: [Scanner] -> [FullScanner]
toFullScanners [] = []
toFullScanners (x:xs) = pairAll [(x, (0, 0, 0))] (map orintations xs)

pairAll :: [FullScanner] -> [[Scanner]] -> [FullScanner]
pairAll paired [] = paired
pairAll paired (x:xs) =
  case foldl (\acc new -> 
    case acc of 
      Just p -> Just p
      Nothing -> foldl (\acc p -> case acc of 
        Just p -> Just p 
        Nothing -> pair p new
        ) Nothing x
    ) Nothing paired of 
    Just s -> pairAll (s:paired) xs
    Nothing -> pairAll paired (xs ++ [x]) 

pair :: Scanner -> FullScanner -> Maybe FullScanner
pair s1 (s2, (px, py, pz)) = 
  let 
    ss = map 
      (\(x, y, z) -> 
        (Set.map (\(xs, ys, zs) -> (xs - x, ys - y, zs - z)) s1,
        (negate x, negate y, negate z))) 
      (offsets s1 s2)
  in 
    find ((>= 12) . Set.size . Set.intersection s2 . fst) ss

offsets :: Scanner -> Scanner -> [Point]
offsets s1 s2 = 
  concatMap (\(x1, y1, z1) -> map (\(x2, y2, z2) -> (x1 - x2, y1 - y2, z1 - z2)) (Set.toList s2)) (Set.toList s1)

orintations :: Scanner -> [Scanner]
orintations scanner = 
  let
    ps = map mySequence $ Set.toList scanner
  in 
    map (\i -> Set.fromList $ map (!! i) ps) [0..23]

mySequence :: Point -> [Point]
mySequence p =
  let 
    turns = take 4 . iterate turn
    steps p = mapAccumL (\p _ -> 
      let 
        ps = turns (roll p)
      in 
        (last ps, ps)
      ) p [1..3]
    (newP, cycleA) = steps p 
    (_, cycleB) = steps . roll . turn $ roll newP
  in
    concat (cycleA ++ cycleB)

roll ::Point -> Point
roll (x, y, z) = (x, z, negate y) 

turn :: Point -> Point
turn (x, y, z) = (negate y, x, z)

pointToList :: Point -> [Int]
pointToList (x,y,z) = [x, y, z]

listToPoint :: [Int] -> Point
listToPoint [x,y,z] = (x, y, z)

parse :: String -> [Scanner]
parse = map parseScanner . splitOn "\n\n"

parseScanner :: String -> Scanner
parseScanner = Set.fromList . map parsePoint . tail . lines

parsePoint :: String -> Point
parsePoint = listToPoint . map read . splitOn ","
