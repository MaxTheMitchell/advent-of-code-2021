import Data.List ( sort, group )
import Data.List.Split ( splitOn )
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
type RuleKey = (Char, Char)
type Rules = Map.Map RuleKey Char
type CharCount = Map.Map Char Int
type RuleCache = Map.Map (RuleKey, Int) CharCount 

silverBody :: String -> String 
silverBody str = 
  let 
    (s, rules) = parse str
    lens = map length . group . sort $ foldl (\acc _ -> applyRules rules acc) s [1..10]
  in
    show (maximum lens - minimum lens)

goldBody :: String -> String
goldBody str = 
  let 
    (s, rules) = parse str
    lens = expandAll 40 rules s  
  in
    show (maximum lens - minimum lens)

applyRules :: Rules -> String -> String 
applyRules rules (a:b:str) = 
  case Map.lookup (a,b) rules of 
    Nothing -> a:applyRules rules (b:str)
    Just v ->  a:v:applyRules rules (b:str)
applyRules _ str = str

expandAll :: Int -> Rules -> String -> CharCount
expandAll i rules str = 
  fst $
  foldl (\(cc, rc) new -> 
    let 
      (newCC, newRc) = expand rc i rules new
    in 
      (Map.unionWith (+) cc newCC, newRc)) 
    (Map.singleton (last str) 1, Map.empty)
    (zip str (tail str)) 

expand :: RuleCache -> Int -> Rules -> RuleKey -> (CharCount, RuleCache)
expand rc 0 _ (a, _) = (Map.singleton a 1, rc)
expand rc n rules k@(a, b) = 
  case Map.lookup (k, n) rc of
    Just cc -> (cc, rc)
    Nothing -> 
      case Map.lookup k rules of 
        Nothing -> (Map.singleton a 1, rc)
        Just c -> 
          let 
            (cc1, rc1) = expand rc (n - 1) rules (a, c)
            (cc2, finalRc) = expand rc1 (n - 1) rules (c, b)
            finalCc = Map.unionWith (+) cc1 cc2
          in 
            (finalCc, Map.insert (k, n) finalCc finalRc)

parse :: String -> (String, Rules)
parse str = 
  let 
    [s, rules] = splitOn "\n\n" str
  in
    (s,
    Map.fromList . map ((\[[a,b], [c]] -> ((a,b), c) ). splitOn " -> ") $ lines rules
    )