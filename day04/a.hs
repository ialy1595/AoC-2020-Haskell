import System.IO( Handle, IOMode( ReadMode, WriteMode ), 
                  openFile, hGetLine, hClose, hIsEOF, hPutStr )

rInt :: String -> Int
rInt = read

pStr :: Int -> String
pStr = show

splitPassport :: [String] -> [String]
splitPassport [] = [""]
splitPassport (c:cs)
    | c == "" = "" : rest
    | otherwise = (c ++ " " ++ head rest) : tail rest
    where
        rest = splitPassport cs

parsePassport :: String -> [String]
parsePassport "" = [""]
parsePassport " " = [""]
parsePassport (c:cs)
    | c == ' ' = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = parsePassport cs

parseFields :: String -> [String]
parseFields [] = [""]
parseFields (c:cs)
    | c == ':' = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = parseFields cs

elemR :: Eq a => [a] -> a -> Bool
elemR ss s = s `elem` ss

checkHasField :: [String] -> [[String]] -> Bool
checkHasField fs ps = all (elemR $ map head ps) fs

between :: Int -> Int -> Int -> Bool
between x y z = y <= x && x <= z

isNumber :: String -> Bool
isNumber = all (elemR "0123456789")

checkHeight :: String -> Bool
checkHeight s
    | length s < 3 = False
    | un == "cm" = isNumber vl && between (rInt vl) 150 193
    | un == "in" = isNumber vl && between (rInt vl) 59 76
    | otherwise = False
    where
        vl = take (length s - 2) s
        un = drop (length s - 2) s

checkHcl :: String -> Bool
checkHcl ('#':cs) = all (elemR "0123456789abcdef") cs
checkHcl _ = False

checkEachField :: [[String]] -> Bool
checkEachField [] = True
checkEachField (s:ss)
    | field == "byr" = isNumber vl && between (rInt vl) 1920 2002 && rest
    | field == "iyr" = isNumber vl && between (rInt vl) 2010 2020 && rest
    | field == "eyr" = isNumber vl && between (rInt vl) 2020 2030 && rest
    | field == "hgt" = checkHeight vl && rest
    | field == "hcl" = checkHcl vl && rest
    | field == "ecl" = vl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] && rest
    | field == "pid" = length vl == 9 && all (elemR "0123456789") vl && rest
    | field == "cid" = rest
    | otherwise = rest
    where
        rest = checkEachField ss
        field = head s
        vl = last s

f :: [String] -> [String] -> Int
f s fs = length $ filter checkEachField $ filter (checkHasField fs) (map (map parseFields . parsePassport) $ splitPassport s)

handleTotal :: Handle -> Handle -> [String] -> IO ()
handleTotal handleI handleO prevInput = do
    end <- hIsEOF handleI
    if end then
       hPutStr handleO $ pStr $ f prevInput ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    else do
       line <- hGetLine handleI
       handleTotal handleI handleO $ prevInput ++ [line]

main :: IO ()
main = do
    handleI <- openFile "input.txt" ReadMode
    handleO <- openFile "output.txt" WriteMode
    handleTotal handleI handleO []
    hClose handleI
    hClose handleO