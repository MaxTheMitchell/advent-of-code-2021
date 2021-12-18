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

data SnailNumber
  = Branch SnailNumber SnailNumber
  | Leaf Int
  deriving Eq

instance Show SnailNumber where
  show (Branch sn1 sn2) = "[" ++ show sn1 ++ "," ++ show sn2 ++ "]"
  show (Leaf i) = show i 

silverBody :: String -> String 
silverBody = show . magnitude . sumSnailNumbers . parse

goldBody :: String -> String
goldBody str = 
  let 
    sns = parse str 
  in 
    show $ maximum [magnitude $ addSnailNumbers sn1 sn2 | sn1<-sns, sn2<-sns]

magnitude :: SnailNumber -> Int 
magnitude (Branch l r) = 3 * magnitude l + 2 * magnitude r
magnitude (Leaf l) = l  

sumSnailNumbers :: [SnailNumber] -> SnailNumber
sumSnailNumbers = foldl1 addSnailNumbers

addSnailNumbers :: SnailNumber -> SnailNumber -> SnailNumber
addSnailNumbers sn1 sn2 = reduce $ Branch sn1 sn2 

reduce :: SnailNumber -> SnailNumber
reduce sn = 
  case (explodeWrapper sn, split sn) of 
    ((newSn, True), _) -> reduce newSn
    (_, (newSn, True)) -> reduce newSn
    _ -> sn

explodeWrapper :: SnailNumber -> (SnailNumber, Bool)
explodeWrapper sn = 
  let 
    (newSn, i, (l, r)) = explode 0 0 sn
    finalSn = addAtIndexWrapper (i - 1) l $ addAtIndexWrapper (i + 1) r newSn 
  in
    if (l, r) == lrFake 
      then (sn, False)
      else (finalSn, True)

explode :: Int -> Int -> SnailNumber -> (SnailNumber, Int, (Int, Int))
explode i depth sn@(Branch (Leaf l) (Leaf r))
  | depth > 3 = (Leaf 0, i, (l, r))
  | otherwise = (sn, i + 2, lrFake)
explode i depth (Branch lb rb) = 
  let 
    (newLb, iLeft, lrLeft) = explode i (depth + 1) lb
    (newRb, iRight, lrRight) = explode iLeft (depth + 1) rb
    (finalLB, finalRB, iFinal, lrFinal) = if lrLeft /= lrFake 
      then (newLb, rb, iLeft, lrLeft)
      else (newLb, newRb, iRight, lrRight)
  in
    (Branch finalLB finalRB, iFinal, lrFinal)
explode i _ l@(Leaf _) = (l, i + 1, lrFake)

lrFake :: (Int, Int)
lrFake = (-1, -1)

addAtIndexWrapper :: Int -> Int -> SnailNumber -> SnailNumber
addAtIndexWrapper i n sn = fst $ addAtIndex i n sn 

addAtIndex :: Int -> Int -> SnailNumber -> (SnailNumber, Int)
addAtIndex (-1) _ sn = (sn, -1) 
addAtIndex i n (Leaf l)
  | i == 0 = (Leaf (n + l), i - 1)
  | otherwise = (Leaf l, i - 1) 
addAtIndex i n (Branch l r) = 
  let 
    (newL, newI) = addAtIndex i n l
    (newR, finalI) = addAtIndex newI n r 
  in 
    (Branch newL newR, finalI) 

split :: SnailNumber -> (SnailNumber, Bool)
split (Leaf l)
  | l > 9 =
    let float = toRational l / 2 in  
    (Branch (Leaf $ floor float) (Leaf $ ceiling float), True)
  | otherwise = (Leaf l, False)
split (Branch l r) = 
  case (split l, split r) of 
    ((newL, True), _) -> (Branch newL r, True)
    (_, (newR, True)) -> (Branch l newR, True)
    _                 -> (Branch l r, False) 

parse :: String -> [SnailNumber]
parse = map (reduce . parseSnailNum) . lines 

parseSnailNum :: String -> SnailNumber
parseSnailNum ('[':str) = 
  let 
    firstSnail open closer str (x:xs) = 
      case x of 
        '[' -> firstSnail (open + 1) closer (str ++ [x]) xs 
        ']' -> firstSnail open (closer + 1) (str ++ [x]) xs
        _   -> if open == closer && x == ','
          then (str, init xs)
          else firstSnail open closer (str ++ [x]) xs
    (first, last) = firstSnail 0 0 "" str 
  in
    Branch (parseSnailNum first) (parseSnailNum last)
parseSnailNum str = Leaf $ read str  
