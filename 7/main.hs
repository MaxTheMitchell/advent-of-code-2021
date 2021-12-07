import Data.List.Split ( splitOn )

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

silverBody :: String -> String 
silverBody str =
  let 
    crabs = parse str
    minPos = minimum crabs
    maxPos = maximum crabs
  in 
    show 
    . minimum 
    . map (findGas crabs)
    $ [minPos..maxPos]    
  
findGas :: [Int] -> Int -> Int
findGas crabs pos = 
  sum $ map (abs . (-) pos) crabs

goldBody :: String -> String
goldBody str = 
  let 
    crabs = parse str
    minPos = minimum crabs
    maxPos = maximum crabs
  in 
    show 
    . minimum 
    . map (findGasGold crabs)
    $ [minPos..maxPos]    

findGasGold :: [Int] -> Int -> Int
findGasGold crabs pos = 
  sum $ map (sumation . abs . (-) pos) crabs

sumation :: Int -> Int 
sumation n = sum [0..n]

parse :: String -> [Int]
parse = map read . splitOn ","