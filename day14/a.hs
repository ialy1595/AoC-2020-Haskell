import System.IO( Handle, IOMode( ReadMode, WriteMode ), 
                  openFile, hGetLine, hClose, hIsEOF, hPutStr )

import Data.List.Split( splitOn )
import Data.List( isPrefixOf )
import Data.Map (Map, elems, insert, empty )

rInt :: String -> Int
rInt = read

pStr :: Int -> String
pStr = show

make36Binary :: String -> Int -> String
make36Binary prev now
   | length prev == 36 = prev
   | otherwise = make36Binary (nowBit:prev) (now `div` 2)
   where 
      nowBit = if now `mod` 2 == 1 then '1' else '0'

calc36Binary :: Int -> String -> Int
calc36Binary prev "" = prev
calc36Binary prev (c:cs) = calc36Binary (prev * 2 + rInt [c]) cs

parseProgram :: String -> (Int, String)
parseProgram s
   | "mask" `isPrefixOf` s = (-1, drop 7 s)
   | otherwise = ((rInt . drop 4 . head) sp, (make36Binary "" . rInt . last) sp)
   where
      sp = splitOn "] = " s

masking :: Char -> Char -> Char
masking '0' _ = '0'
masking '1' _ = '1'
masking _ x = x

maskValue :: String -> String -> String
maskValue = zipWith masking

solveProgram :: Map Int String -> String -> [(Int, String)] -> Int
solveProgram mp mask [] = (sum . map (calc36Binary 0) . elems) mp
solveProgram mp mask ((i, s):iss)
   | i == -1 = solveProgram mp s iss
   | otherwise = solveProgram (insert i (maskValue mask s) mp) mask iss 

handleTotal :: Handle -> Handle -> [String] -> IO ()
handleTotal handleI handleO prevInput = do
    end <- hIsEOF handleI
    if end then
       hPutStr handleO $ pStr $ solveProgram empty "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" $ map parseProgram prevInput
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