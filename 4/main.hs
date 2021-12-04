import Data.List ( find )
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

type Board = [[Int]]

silverBody :: String -> String 
silverBody str =  show $ silverAnws (parseBoards str) (parseNums str)

goldBody :: String -> String
goldBody str = show $ goldAnws (parseBoards str) (parseNums str)

silverAnws :: [Board] -> [Int] -> Int 
silverAnws boards nums =  
  (\(Just board, nums) -> sum (remNums board nums ) * head nums)
  $
  foldl (\acc new -> 
      case acc of
        res@(Just _, _) -> res
        (Nothing, xs) -> (findWinner boards (new:xs), new:xs)
    ) 
    (Nothing, []) 
    nums

goldAnws :: [Board] -> [Int] -> Int 
goldAnws boards nums =  
  (\(_, [b]) -> silverAnws [b] nums)
  $
  foldl (\acc new -> 
      case acc of
        res@(_, [b]) -> res
        (xs, bs) -> 
          (
            new:xs, 
            filter (\b -> not $ isWinner b (new:xs)
            ) bs 
          )
    ) 
    ([], boards) 
    nums

remNums :: Board -> [Int] -> [Int]
remNums board nums = filter (`notElem` nums) $ concat board    

findWinner :: [Board] -> [Int] -> Maybe Board
findWinner boards nums = find (`isWinner` nums) boards

isWinner :: Board -> [Int] -> Bool
isWinner board nums = vertWin board nums || horWin board nums

vertWin :: Board -> [Int] -> Bool
vertWin board nums = any (all (`elem` nums)) board 

horWin :: Board -> [Int] -> Bool
horWin board =  vertWin (rotate board)

rotate :: Board -> Board
rotate board@(b:_) = map (\i -> map (!! i) board) [0..(length b - 1)]

parseNums :: String -> [Int]
parseNums = map read . splitOn "," . head . lines 

parseBoards :: String -> [Board]
parseBoards = map (map (map read . words) . lines ) . tail . splitOn "\n\n"