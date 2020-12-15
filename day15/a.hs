import System.IO( Handle, IOMode( ReadMode, WriteMode ), 
                  openFile, hGetLine, hClose, hIsEOF, hPutStr )

import Data.List.Split( splitOn )
import Data.List( isPrefixOf )
import Data.Map (Map, elems, insert, empty, member, (!) , fromList)

rInt :: String -> Int
rInt = read

pStr :: Int -> String
pStr = show

parseNumber :: String -> [Int]
parseNumber  = map rInt . splitOn ","

f :: Map Int Int -> Int -> Int -> Int
f mp x idx
   | idx == 30000000 = x
   | otherwise = (f $! insert x idx mp) nextVal (idx + 1)
   where
      nextVal = if member x mp then idx - (mp ! x) else 0

handleTotal :: Handle -> Handle -> [String] -> IO ()
handleTotal handleI handleO prevInput = do
    end <- hIsEOF handleI
    if end then
       let is = parseNumber (head prevInput)
       in hPutStr handleO $ pStr $ f (fromList (zip (init is) [1..])) (last is) (length is)
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