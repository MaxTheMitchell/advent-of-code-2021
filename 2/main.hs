import Data.List
-- import Data.Char
import Data.Maybe
-- import Text.Regex

data Command 
    = Forward Int 
    | Down Int 
    | Up Int
    deriving Show

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
silverBody = 
    show 
    . uncurry (*)
    . foldl (\(x, y) cmd -> case cmd of 
        Forward i-> (x+i, y)
        Down i -> (x, y+i)
        Up i -> (x, y-i)    
        
    ) (0,0)
    .  parse

goldBody :: String -> String
goldBody =
    show 
    . (\(x,y,_) -> x*y)
    . foldl (\(x, y, a) cmd -> case cmd of 
        Forward i-> (x+i, y + a*i, a)
        Down i -> (x, y, a+i)
        Up i -> (x, y, a-i)    
        
    ) (0,0,0)
    .  parse

parse :: String -> [Command]
parse = map (
    (\[cmd, i] -> 
        (\f -> f (read i))
        . snd 
        . fromJust 
        $ find ((== cmd) . fst) [
        ("forward", Forward),
        ("down", Down),
        ("up", Up)
    ])
    . words)  . lines
