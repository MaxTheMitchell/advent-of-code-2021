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
type Octops = Map.Map Point Int 

silverBody :: String -> String 
silverBody str = 
  show
  . snd 
  $ foldl (\(oct, i) _ -> 
    let 
      newOct = step2 $ step1 oct
    in 
      (newOct, i + nFlashes newOct)   
  ) (parse str, 0) [1..100]

goldBody :: String -> String
goldBody =
  show
  . findAllFlash 1
  . parse 

findAllFlash :: Int -> Octops -> Int 
findAllFlash i oct = 
  let 
    newOct = step2 $ step1 oct
  in 
    if nFlashes newOct == 100
    then i 
    else findAllFlash (i + 1) newOct

nFlashes :: Octops -> Int
nFlashes = length . filter (== 0 ) . Map.elems

step1 :: Octops -> Octops
step1 = Map.fromList . map (\(p, i) -> (p, i + 1)) . Map.toList

step2 :: Octops -> Octops
step2 oct = 
  let
    flashes = findFlashes oct
    newOct = (`reset` flashes) $ applyFlashes oct flashes 
  in
    if any (> 9) $ Map.elems newOct 
    then step2 newOct 
    else newOct

reset :: Octops -> [Point] -> Octops
reset = foldl (\acc p -> Map.insert p 0 acc)

applyFlashes :: Octops -> [Point] -> Octops
applyFlashes = foldl applyFlash

applyFlash :: Octops -> Point -> Octops
applyFlash oct (x, y) = foldl increatesFromFlash oct [(x1, y1) | x1 <-[x-1..x+1], y1 <-[y-1..y+1]]

increatesFromFlash :: Octops -> Point -> Octops
increatesFromFlash oct p = 
  case Map.lookup p oct of
    Nothing -> oct
    Just 0 -> oct 
    Just v -> Map.insert p (v + 1) oct

findFlashes :: Octops -> [Point]
findFlashes = map fst . filter ((> 9) . snd) . Map.toList

parse :: String -> Octops
parse = Map.fromList 
  . concatMap (\(s , y) -> map (\(c, x) -> ((x, y), read [c])) (zip s [0..]))
  . (`zip` [0..]) 
  . lines