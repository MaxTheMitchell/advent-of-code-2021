import Data.List.Split (splitOn)
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

type FishMap = Map.Map Int Int

silverBody :: String -> String 
silverBody str =
  let 
      init = parse str
  in 
    show . length $ foldl (\acc _ -> dayPasses acc ) init [1..80]

goldBody :: String -> String
goldBody str =
  let 
      init = parse str
  in 
    show
    . snd
    . foldl (\(fm, n) day-> (\(fm, new) -> (fm, new + n)) $ fish fm day) (Map.empty, 0) 
    $ map (\n -> (256 + 9) - n) init

fish :: FishMap -> Int -> (FishMap, Int)
fish fm day =
  case Map.lookup day fm of   
    Just v -> (fm, v)
    Nothing -> 
      (\(newFm, n) -> (Map.insert day n newFm, n))
      . foldl (\(fmap, n) day ->  
        let 
          (newFmap, newN) = fish fmap day 
        in 
          (newFmap, newN + n)
      ) (fm, 1)
      . takeWhile (> 0)
      $ iterate (\n -> n - 7) (day-9)

dayPasses :: [Int] -> [Int]
dayPasses [] = []
dayPasses (0:xs) = [6, 8] ++ dayPasses xs
dayPasses (n:xs) = (n - 1):dayPasses xs

parse :: String -> [Int]
parse = map read . splitOn "," . concat . words