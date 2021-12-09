import Data.List ( find, groupBy, partition, sort, sortOn )
import Data.List.Split ( splitOn )
import Data.Maybe ( fromJust )

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
    outputs = concatMap snd $ parse str   
  in 
    show 
    . length 
    . filter (/= 6)
    . filter (/= 5)
    $ map length outputs

goldBody :: String -> String
goldBody =
  show  
  . sum 
  . map decode 
  . parse

parse :: String -> [([String], [String])]
parse = map ((\[a, b] -> (words a, words b)). splitOn " | ") . lines

decode :: ([String], [String]) -> Int
decode (input, output) = decodeOutput output (decodeInput input) 

decodeOutput :: [String] -> [(Char, String)] -> Int
decodeOutput strs keys = 
  read $ map ((\str -> fst . fromJust $ find ((== str ) . snd ) keys) . sort) strs

decodeInput :: [String] -> [(Char, String)]
decodeInput s = 
  let 
    [[one], [seven], [four], nums235, nums690, [eight]] = 
      groupBy (\a b -> length a == length b)
      . sortOn length
      . map sort
      $ sort s
  in  
    [
      ('1', one),
      ('7', seven),
      ('4', four),
      ('8', eight)
    ] ++ decode690 nums690 one four ++ decode235 nums235 one four 

decode690 :: [String] -> String -> String -> [(Char, String)]
decode690 strs690 one four =
  let 
    (rest, [six]) = partition (\str -> all (`elem` str) one) strs690
    ([nine], [zero]) = partition (\str -> all (`elem` str) four) rest
  in
    [
      ('0', zero),
      ('6', six),
      ('9', nine)
    ]

decode235 :: [String] -> String -> String -> [(Char, String)]
decode235 strs235 one four = 
  let 
    ([three], rest) = partition (\str -> all (`elem` str) one) strs235
    ([two], [five]) = partition (\str -> (== 2) . length $ filter (`elem` str) four) rest
  in 
    [
      ('2', two),
      ('3', three),
      ('5', five)
    ]
