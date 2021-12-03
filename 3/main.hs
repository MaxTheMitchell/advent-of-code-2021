import Data.List ( group, groupBy, sort, sortOn )

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
silverBody = show . (\xs -> binToInt (findGamma xs) * binToInt (epsilon xs)) . parse

goldBody :: String -> String
goldBody str = 
    let 
      ls = lines str 
    in 
      show (findOX 0 ls * findCO 0 ls)    

findGamma :: [String] -> String
findGamma = map ((\[a, b] -> if a > b then '1' else '0') .  map length . group . sort)

epsilon ::  [String] -> String
epsilon = map (\c -> if c =='1' then '0' else '1') . findGamma

binToInt :: String -> Int 
binToInt "" = 0
binToInt (x:xs)
  | x == '1' = 2 ^ length xs + binToInt xs
  | otherwise = binToInt xs

findOX :: Int -> [String] -> Int 
findOX _ [str] = binToInt str
findOX i strs = findOX (i+1) (remaingNums i strs)

findCO :: Int -> [String] -> Int 
findCO _ [str] = binToInt str
findCO i strs = findCO (i+1) (remaingNumsCO i strs)

remaingNumsCO :: Int -> [String] -> [String]
remaingNumsCO i strs = (\c -> filter (\str -> (str !! i) /= c) strs ) . (\(z, o) -> if z > o then '0' else '1') $ oneOrZero i strs 

remaingNums :: Int -> [String] -> [String]
remaingNums i strs = (\c -> filter (\str -> (str !! i) == c) strs ) . (\(z, o) -> if z > o then '0' else '1') $ oneOrZero i strs 

oneOrZero :: Int -> [String] -> (Int, Int) 
oneOrZero i = (\[z, o] -> (z , o)) . map length . group . sort . map (!! i)  

parse :: String -> [String]
parse str = 
    let 
      len = length . head $ lines str
    in 
      map (map snd) . groupBy (\(a,_) (b,_) -> a == b ) . sortOn fst . zip (cycle [1..len]). concat . words $ str
