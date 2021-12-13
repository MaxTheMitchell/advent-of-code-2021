import Data.List ( intercalate )
import Data.List.Split ( splitOn )
import qualified Data.Set as Set 

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
data Direction = X | Y
type Fold = (Direction, Int)
type Point = (Int, Int)
type Points = Set.Set Point

silverBody :: String -> String 
silverBody str =
  let 
    (x:_) = parseFolds str
  in 
    show
    . Set.size 
    $ foldAll (parsePoints str) [x]

goldBody :: String -> String
goldBody str = showPoints $ foldAll (parsePoints str) (parseFolds str) 

showPoints :: Points -> String
showPoints ps = 
  let 
    maxX = Set.findMax $ Set.map fst ps 
    maxY = Set.findMax $ Set.map snd ps
  in 
    intercalate "\n" $ map (\y -> map (\x -> if Set.member (x,y) ps then '#' else '.') [0..maxX]) [0..maxY]

foldAll :: Points -> [Fold] -> Points
foldAll = foldl foldAny

foldAny :: Points -> Fold -> Points
foldAny ps (X, i) = foldX ps i 
foldAny ps (Y, i) = foldY ps i 

foldX :: Points -> Int -> Points
foldX points i = 
  let 
    addPoint (x, y)
      | (x > i) = (x + (i - x) * 2, y)
      | otherwise = (x, y)
  in 
    Set.map addPoint points

foldY :: Points -> Int -> Points
foldY points i = 
  let 
    addPoint (x, y)
      | (y > i) = (x, y + (i - y) * 2)
      | otherwise = (x, y)
  in 
    Set.map addPoint points

parseFolds :: String -> [Fold]
parseFolds = 
  map (\str -> 
    (if 'x' `elem` str then X else Y,
    (\[_, n] -> read n) $ splitOn "=" str))
  . lines
  . (!! 1)
  . splitOn "\n\n"

parsePoints :: String -> Points
parsePoints = 
  Set.fromList
  . map ((\[a, b] -> (a,b)) . map read . splitOn ",")
  . lines
  . head 
  . splitOn "\n\n"
