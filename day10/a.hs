import System.IO( Handle, IOMode( ReadMode, WriteMode ), 
                  openFile, hGetLine, hClose, hIsEOF, hPutStr )

import Data.List( sort )

rInt :: String -> Int
rInt = read

pStr :: Int -> String
pStr = show

addSE :: [Int] -> [Int]
addSE as = [0] ++ as ++ [last as + 3]

f :: Int -> Int -> [Int] -> Int
f x y (a1:a2:as)
    | diff == 1 = f (x + 1) y (a2:as)
    | diff == 2 = f x y (a2:as)
    | diff == 3 = f x (y + 1) (a2:as)
    where
        diff = a2 - a1
f x y _ = x * y

handleTotal :: Handle -> Handle -> [String] -> IO ()
handleTotal handleI handleO prevInput = do
    end <- hIsEOF handleI
    if end then
       hPutStr handleO $ pStr $ f 0 0 $ addSE $ sort $ map rInt prevInput
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