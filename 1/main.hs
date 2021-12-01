main :: IO ()
main = silver input >> gold input

gold :: IO String -> IO ()
gold = (>>= putStrLn . (++) "gold:\n" . goldBody)

silver :: IO String -> IO () 
silver = (>>= putStrLn . (++) "silver:\n" . silverBody)

test :: IO String
test = readFile "test.txt"

input :: IO String
input = readFile "input.txt"

---------------------------------------------------------------------

silverBody :: String -> String 
silverBody = show . length . filter (uncurry (<)) . (\a -> zip a (tail a) ) . parse

goldBody :: String -> String
goldBody = show . length . filter (uncurry (<)) .  (\a -> zip a (tail a) ) . map (\(a, b, c) -> a+b+c) . (\a -> zip3 a (tail a) (tail (tail a))) . parse

parse :: String -> [Int]
parse = map read . lines

