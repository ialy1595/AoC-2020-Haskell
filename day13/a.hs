import System.IO( Handle, IOMode( ReadMode, WriteMode ), 
                  openFile, hGetLine, hClose, hIsEOF, hPutStr )

import Data.List.Split( splitOn )
import Data.List( minimumBy )

rInt :: String -> Int
rInt = read

pStr :: Int -> String
pStr = show

parseIDs :: String -> [Int]
parseIDs s = map rInt (filter ("x" /= ) (splitOn "," s))

compFst :: (Int, Int) -> (Int, Int) -> Ordering
compFst x y = compare (fst x) (fst y)

findFirst :: Int -> Int -> (Int, Int)
findFirst x y = (y - ((x - 1) `mod` y + 1), y * (y - ((x - 1) `mod` y + 1)))

handleTotal :: Handle -> Handle -> [String] -> IO ()
handleTotal handleI handleO prevInput = do
    end <- hIsEOF handleI
    if end then
       hPutStr handleO $ pStr $ snd $ minimumBy compFst $ map ((findFirst . rInt . head) prevInput) ((parseIDs . head . tail) prevInput)
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