import System.IO( Handle, IOMode( ReadMode, WriteMode ), 
                  openFile, hGetLine, hClose, hIsEOF, hPutStr )

import Data.List( sort )

rInt :: String -> Int
rInt = read

pStr :: Int -> String
pStr = show

decode :: Int -> Char -> String -> Int
decode prev _ [] = prev
decode prev a (c:cs)
    | a == c = decode (prev * 2 + 1) a cs
    | otherwise = decode (prev * 2) a cs

f :: String -> Int
f s = decode 0 'B' fb * 8 + decode 0 'R' lr
    where
        fb = take 7 s
        lr = drop 7 s

findSeat :: [Int] -> Int
findSeat (c1:cs)
    | head cs - c1 == 2 = c1 + 1
    | otherwise = findSeat cs

handleTotal :: Handle -> Handle -> [String] -> IO ()
handleTotal handleI handleO prevInput = do
    end <- hIsEOF handleI
    if end then
       hPutStr handleO $ pStr $ findSeat $ sort $ map f prevInput
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