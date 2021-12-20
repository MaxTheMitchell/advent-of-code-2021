import Data.List.Split ( splitOn )
import qualified Data.Set as Set 
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
type Image = Map.Map Point Bool
type Decoder = Set.Set Int

silverBody :: String -> String 
silverBody =
  show . nLights . uncurry enhanceTwice . parse

goldBody :: String -> String
goldBody str = 
  let (decode, img) = parse str
  in show . nLights $ foldl (\acc _ -> enhanceTwice decode acc ) img [1..25]

nLights :: Image -> Int
nLights = length . filter id . Map.elems

enhanceTwice :: Decoder -> Image -> Image
enhanceTwice decode img =  newImage (Set.member 0 decode) decode $ newImage False decode img

newImage :: Bool -> Decoder -> Image -> Image
newImage defualt decode img = 
  Map.fromList . map (\p -> (p, validPoint defualt decode img p)) . Set.toList $ createPoints img

validPoint :: Bool -> Decoder -> Image -> Point -> Bool 
validPoint defualt decode img = 
  (`Set.member` decode) . binToInt . map (\p -> Map.findWithDefault defualt p img) . pointBox

binToInt :: [Bool] -> Int 
binToInt [] = 0
binToInt (x:xs)
  | x = 2 ^ length xs + binToInt xs
  | otherwise = binToInt xs


pointBox :: Point -> [Point]
pointBox (x, y) = [(x+xx, y+yy) | yy<-[-1..1], xx<-[-1..1]]

createPoints :: Image -> Set.Set Point
createPoints = 
  foldl1 Set.union
  . map (\(x, y) -> 
    Set.fromList [(x+xx, y+yy) | xx<-[-2..2], yy<-[-2..2]]  
  )
  . Map.keys

parse :: String -> (Decoder, Image)
parse str = 
  let [decode, img] = splitOn "\n\n" str
  in (parseDecoder decode, parseImage img) 

parseDecoder :: String -> Decoder 
parseDecoder = Set.fromList . map fst . filter ((== '#') . snd) . zip [0..]

parseImage :: String -> Image
parseImage = 
  Map.fromList . 
  concatMap (\(y, str) -> 
    map (\(x, c) -> ((x, y), c == '#')) $ zip [0..] str) 
  . zip [0..]
  . lines 