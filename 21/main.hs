import Data.List.Split ( splitOn )
import Data.List ( permutations )
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

type Pos = Int
type Score = Int
type Player = (Pos, Score)
type Players = (Player, Player)
type GameState = (Players, [Int], Int)
type Cache = Map.Map (Bool, Players) (Integer, Integer)

silverBody :: String -> String 
silverBody str = show $ playGame (parse str, deterministicDie, 0) 

goldBody :: String -> String
goldBody = show . uncurry max . fst . playGoldGame Map.empty True . parse

playGame :: GameState -> Int
playGame (((pos, score), player2@(_, p2Score)), a:b:c:die, rounds) = 
  let 
    newPos = ((pos + a + b + c - 1) `mod` 10) + 1
    newScore =  score + newPos
    newRounds = rounds + 3
  in 
    if newScore >= 1000 
      then p2Score * newRounds
      else
        playGame ((player2, (newPos, newScore)), die, newRounds) 
playGame _ = 0

playGoldGame :: Cache -> Bool -> Players -> ((Integer, Integer), Cache)
playGoldGame cache p1Turn players@(p1@(_, p1Score), p2@(_, p2Score)) =
  case Map.lookup (p1Turn, players) cache of
    Just v -> (v, cache)
    Nothing -> case (p1Score >= 21, p2Score >= 21) of
      (True, _)      -> ((1, 0), cache)
      (False, True)  -> ((0, 1), cache)
      (False, False) ->
        let 
          playersMove i = if p1Turn then (movePlayer i p1, p2) else (p1, movePlayer i p2)
          (wins, finalCache) = foldl 
            (\((w1, w2), cache) i -> 
              let ((nw1, nw2), newCache) = playGoldGame cache (not p1Turn) (playersMove i)
              in ((w1 + nw1, w2 + nw2), newCache)
            ) ((0,0), cache) quatiumDie
        in 
          (wins, Map.insert (p1Turn, players) wins finalCache)

movePlayer :: Int -> Player -> Player
movePlayer i (pos, score) =
  let newPos = ((pos + i - 1) `mod` 10) + 1
  in (newPos, score + newPos)

quatiumDie :: [Int]
quatiumDie = [a+b+c | a<-[1..3], b<-[1..3], c<-[1..3]]

deterministicDie :: [Int]
deterministicDie = cycle [1..100]

parse :: String -> Players
parse = (\[p1, p2] -> ((p1, 0), (p2, 0))) . map (read . last . splitOn ": ") . lines