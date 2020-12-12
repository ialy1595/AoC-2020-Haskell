import System.IO( Handle, IOMode( ReadMode, WriteMode ), 
                  openFile, hGetLine, hClose, hIsEOF, hPutStr )

rInt :: String -> Int
rInt = read

pStr :: Int -> String
pStr = show

parseIns :: String -> (String, Int)
parseIns s
    | head value == '+' = (op, (rInt . tail) value)
    | otherwise = (op, (negate . rInt . tail) value)
    where
        op = take 3 s
        value = drop 4 s

modifyList :: [a] -> Int -> a -> [a]
modifyList as 0 x = x : tail as
modifyList as i x = take i as ++ [x] ++ drop (i+1) as

createChanges :: Int -> [(String, Int)] -> [[(String, Int)]]
createChanges i ins
    | i >= length ins = []
    | op == "jmp" = modifyList ins i ("nop", value) : rest
    | op == "nop" = modifyList ins i ("jmp", value) : rest
    | otherwise = rest
    where
        op = fst (ins !! i)
        value = snd (ins !! i)
        rest = createChanges (i + 1) ins 


f :: Int -> Int -> [Bool] -> [(String, Int)] -> (Bool, Int)
f acu idx seen ins
    | idx >= length ins = (True, acu)
    | seen !! idx = (False, acu)
    | op == "acc" = f (acu + value) (idx + 1) seenCheck ins
    | op == "jmp" = f acu (idx + value) seenCheck ins
    | otherwise = f acu (idx + 1) seenCheck ins
    where
        seenCheck = modifyList seen idx True
        op = fst (ins !! idx)
        value = snd (ins !! idx)
    

handleTotal :: Handle -> Handle -> [String] -> IO ()
handleTotal handleI handleO prevInput = do
    end <- hIsEOF handleI
    if end then
       hPutStr handleO $ pStr $ (snd . head) $ filter fst $ map (f 0 0 (map (const False) [1..(length prevInput)])) (createChanges 0 $ map parseIns prevInput)
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