import Data.List.Split ( splitOn ) 
import Data.Char ( isLower )
import Data.Map as Map ((!), findWithDefault, empty, insert, Map, insertWith, toList)
import qualified Data.Set as Set

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
type Paths = Map.Map String (Set.Set String)

silverBody :: String -> String 
silverBody str = show  $ nPaths Set.empty (parse str) "start"  

goldBody :: String -> String
goldBody str = show  $ nPathsGold False Set.empty (parse str) "start" 

nPaths:: Set.Set String -> Paths -> String -> Int
nPaths _ _ "end" = 1
nPaths used paths key =
  let 
    newUsed = if isLower $ head key then Set.insert key used else used 
  in 
    case Set.toList $ Set.difference (paths ! key) used of 
      [] -> 0
      xs -> sum $ map (nPaths newUsed paths) xs

nPathsGold :: Bool -> Set.Set String -> Paths -> String -> Int
nPathsGold _ _ _ "end" = 1
nPathsGold caveUsed used paths key =
  let 
    newUsed = if isLower $ head key then Set.insert key used else used 
    usedSet = if caveUsed then used else Set.empty
  in 
    case Set.toList $ Set.difference (paths ! key) usedSet of 
      [] -> 0
      xs -> sum $ map (\x -> nPathsGold (caveUsed || Set.member x newUsed) newUsed paths x) xs

parse :: String -> Paths
parse str =
  let 
    paths = map ((\[a, b] -> (a, b)) . splitOn "-") $ lines str
    revPaths = map (\(a, b) -> (b, a)) $ filter ((/= "start") . fst) paths
    insertPaths pMap ps =  
      foldl (\acc (key, val) ->  
        (\set -> Map.insert key set acc) 
        . Set.insert val
        $ Map.findWithDefault Set.empty key acc
      ) pMap ps
  in 
    (`insertPaths` revPaths) $ insertPaths Map.empty paths 