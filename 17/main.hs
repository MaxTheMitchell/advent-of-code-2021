import Data.Array (Ix(inRange))
import Data.List ( groupBy )
import Data.Char ( isDigit )
import Data.Maybe ( mapMaybe )

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
type Velocity = (Int, Int)
type Box = (Pos, Pos)

silverBody :: String -> String 
silverBody = show . findMaxHeight . parse 

goldBody :: String -> String
goldBody = show . findValidVelocities . parse   

findValidVelocities :: Box -> Int
findValidVelocities box@(_, (mx, _)) = length $ mapMaybe (maxHeight box) [(x, y) | x<-[1..mx], y<-[(negate mx)..mx]] 

findMaxHeight :: Box -> Int
findMaxHeight box@(_, (mx, _)) = maximum $ mapMaybe (maxHeight box) [(x, y) | x<-[1..mx], y<-[1..mx]] 

maxHeight :: Box -> Velocity -> Maybe Int
maxHeight box vel = 
  let 
   ps = map snd
      . takeWhile (not . pastBox box . snd)
      $ iterate
        (\(v, p) -> (applyDrag v, applyVelocity v p))
        (vel, (0, 0))
  in 
    if any (inBox box) ps 
      then Just . maximum $ map snd ps
      else Nothing

applyDrag :: Velocity -> Velocity
applyDrag (x, y) = (max 0 (x - 1), y - 1)

applyVelocity :: Velocity -> Pos -> Pos
applyVelocity (vx, vy) (px, py) = (vx + px, vy + py)

pastBox :: Box -> Pos -> Bool
pastBox ((_, my), (mx, _)) (x, y) = x > mx || y < my

inBox :: Box -> Pos -> Bool
inBox = inRange 

parse :: String -> Box
parse str = 
  let 
    isInt c = isDigit c || c == '-'
  in
    (\[x1, x2, y1, y2] -> ((min x1 x2, min y1 y2),(max x1 x2, max y1 y2))) 
    . map read 
    . filter (all isInt) 
    $ groupBy (\a b -> isInt a == isInt b) str 
