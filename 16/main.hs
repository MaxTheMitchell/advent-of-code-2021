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
type Version = Int

data Packet
  = Literal Version Int
  | Operator Version [Packet] ([Int] -> Int)

silverBody :: String -> String 
silverBody = show . versionSum . fst . createPacket . hexStrToBin

goldBody :: String -> String
goldBody = show . packToInt . fst . createPacket . hexStrToBin  

packToInt :: Packet -> Int
packToInt (Literal _ val) = val
packToInt (Operator _ ps f) = f $ map packToInt ps

versionSum :: Packet -> Int
versionSum (Literal v _) = v 
versionSum (Operator v ps _) = v + sum (map versionSum ps) 

createPacket :: String -> (Packet, String)
createPacket str = 
  let 
    (verStr, r) = splitAt 3 str
    (typeStr, rest) = splitAt 3 r
    version = binToInt verStr
  in 
    case binToInt typeStr of 
      4 -> createLiteral version rest
      n -> 
        (\(packs, restFinal) -> (Operator version packs (operatorFunction n), restFinal))
        $ createOperator rest

operatorFunction :: Int -> ([Int] -> Int)
operatorFunction 0 = sum 
operatorFunction 1 = product
operatorFunction 2 = minimum
operatorFunction 3 = maximum
operatorFunction 5 = (\[a, b] -> if a > b then 1 else 0) 
operatorFunction 6 = (\[a, b] -> if a < b then 1 else 0) 
operatorFunction 7 = (\[a, b] -> if a == b then 1 else 0) 

createOperator :: String -> ([Packet], String)
createOperator ('0':str) = totalLenOperator str
createOperator ('1':str) = numSubOperator str

totalLenOperator :: String -> ([Packet], String)
totalLenOperator str = 
  let 
    (len, r1) = splitAt 15 str
    (packsStr, rest) = splitAt (binToInt len) r1
    createPackets "" = []
    createPackets str = (\(p, r) -> p: createPackets r) $ createPacket str
  in
    (createPackets packsStr, rest)

numSubOperator :: String -> ([Packet], String)
numSubOperator str = 
  let 
    (packLen, rest) = splitAt 11 str
  in 
    foldl (\(ps, rest) _ -> (\(p, newRest) -> (ps ++ [p], newRest)) $ createPacket rest ) ([], rest) [1..(binToInt packLen)]

createLiteral :: Version -> String -> (Packet, String)
createLiteral version str = 
  let 
    nums = (\(x:xs, rest) -> 
      case x of
        '0' -> (xs, rest)
        '1' -> ((\(xxs, remain) -> (xs ++ xxs, remain)) $ nums rest)) . splitAt 5
    (ns, rest) = nums str 
  in 
    (Literal version (binToInt ns), rest) 

binToInt :: String -> Int 
binToInt "" = 0
binToInt (x:xs)
  | x == '1' = 2 ^ length xs + binToInt xs
  | otherwise = binToInt xs


hexStrToBin :: String -> String 
hexStrToBin = concatMap hexCharToBin

hexCharToBin :: Char -> String 
hexCharToBin '0' = "0000"
hexCharToBin '1' = "0001"
hexCharToBin '2' = "0010"
hexCharToBin '3' = "0011"
hexCharToBin '4' = "0100"
hexCharToBin '5' = "0101"
hexCharToBin '6' = "0110"
hexCharToBin '7' = "0111"
hexCharToBin '8' = "1000"
hexCharToBin '9' = "1001"
hexCharToBin 'A' = "1010"
hexCharToBin 'B' = "1011"
hexCharToBin 'C' = "1100"
hexCharToBin 'D' = "1101"
hexCharToBin 'E' = "1110"
hexCharToBin 'F' = "1111"
