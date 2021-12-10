import Data.List ( sort )

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
silverBody = show . sum . map (fst . lineScore "") . lines

goldBody :: String -> String
goldBody = 
    show
    . (\l -> l !! (length l `div` 2) )
    . sort 
    . map (remainingScore 0 . snd)
    . filter ((== 0) . fst) 
    . map (lineScore "") 
    . lines

remainingScore :: Int -> String -> Int
remainingScore i "" = i
remainingScore i (x:xs) = remainingScore (5*i + scoreR x) xs 

scoreR :: Char -> Int 
scoreR ')' = 1 
scoreR ']' = 2 
scoreR '}' = 3
scoreR '>' = 4

lineScore :: String -> String -> (Int, String) 
lineScore ps "" = (0, ps)
lineScore [] (x:xs) = case closer x of 
    Just c -> lineScore [c] xs
    Nothing -> (score x, [])
lineScore (p:ps) (x:xs) = case closer x of
    Just c -> lineScore (c:p:ps) xs
    Nothing -> if x == p
        then lineScore ps xs
        else (score x, ps) 

closer :: Char -> Maybe Char 
closer '(' = Just ')'
closer '[' = Just ']'
closer '{' = Just '}'
closer '<' = Just '>'
closer _ = Nothing 
 
score :: Char -> Int 
score ')' = 3 
score ']' = 57 
score '}' = 1197
score '>' = 25137

