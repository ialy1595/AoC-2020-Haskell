import System.IO( Handle, IOMode( ReadMode, WriteMode ), 
                  openFile, hGetLine, hClose, hIsEOF, hPutStr )

import Data.List( sort )

rInt :: String -> Int
rInt = read

pStr :: Int -> String
pStr = show

addE :: [Int] -> [Int]
addE as = as ++ [last as + 3]

between :: Int -> Int -> Int -> Bool
between x y z = y <= x && x <= z

startable :: Int -> Int
startable x
    | x <= 3 = 1
    | otherwise = 0

f :: [Int] -> Int -> Int
f as = memoCountAdapter
    where 
        memoCountAdapter = (map countAdapter [0 ..] !!)
            where
                countAdapter x = sum (map memoCountAdapter jolts) + startable (as !! x)
                    where
                        jolts = filter (\a -> between ((as !! x) - (as !! a)) 1 3) [(maximum [x - 3, 0])..(x - 1)]

handleTotal :: Handle -> Handle -> [String] -> IO ()
handleTotal handleI handleO prevInput = do
    end <- hIsEOF handleI
    if end then
       hPutStr handleO $ pStr $ f (addE $ sort $ map rInt prevInput) (length prevInput)
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