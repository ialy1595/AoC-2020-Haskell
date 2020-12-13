import System.IO( Handle, IOMode( ReadMode, WriteMode ), 
                  openFile, hGetLine, hClose, hIsEOF, hPutStr )

import Data.List.Split( splitOn )
import Data.List( minimumBy )

rInt :: String -> Int
rInt = read

pStr :: Int -> String
pStr = show

xrInt :: String -> Int
xrInt "x" = -1
xrInt x = rInt x

negR :: (Int, Int) -> (Int, Int)
negR (x, r) = (x, negate r `mod` x)

parseIDs :: String -> [(Int, Int)]
parseIDs s = (map negR . filter ((0 <) . fst)) (zip (map xrInt $ splitOn "," s) [0..]) 

modR :: Int -> Int -> Int
modR x y = y `mod` x

findMatch :: (Int, Int) -> (Int, Int) -> (Int, Int)
findMatch (x, xr) (y, yr) = 
   let r = (head . filter ((yr ==) . modR y) . map ((xr +) . (x *))) [0..]
   in (x * y, r)      


findTimestamp :: [(Int, Int)] -> Int
findTimestamp (x1:x2:xs) = findTimestamp (findMatch x1 x2 : xs)
findTimestamp [x] = snd x


handleTotal :: Handle -> Handle -> [String] -> IO ()
handleTotal handleI handleO prevInput = do
    end <- hIsEOF handleI
    if end then
       hPutStr handleO $ pStr $ (findTimestamp . parseIDs . last) prevInput
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