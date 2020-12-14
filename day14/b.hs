import System.IO( Handle, IOMode( ReadMode, WriteMode ), 
                  openFile, hGetLine, hClose, hIsEOF, hPutStr )

import Data.List.Split( splitOn )
import Data.List( isPrefixOf )
import Data.Map (Map, elems, union , empty, fromList )

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

parseProgram :: String -> (String, String)
parseProgram s
   | "mask" `isPrefixOf` s = ("mask", drop 7 s)
   | otherwise = ((make36Binary "" . rInt . drop 4 . head) sp, (make36Binary "" . rInt . last) sp)
   where
      sp = splitOn "] = " s

masking :: Char -> Char -> Char
masking '0' x = x
masking x _ = x

maskValue :: String -> String -> String
maskValue = zipWith masking

generateAddress :: String -> [String]
generateAddress "" = [""]
generateAddress (c:cs)
   | c == 'X' = attachPre '0' ++ attachPre '1'
   | otherwise =  attachPre c
   where
      attachPre x = map (x:) (generateAddress cs)

solveProgram :: Map String String -> String -> [(String, String)] -> Int
solveProgram mp mask [] = (sum . map (calc36Binary 0) . elems) mp
solveProgram mp mask ((s1, s2):sss)
   | s1 == "mask" = solveProgram mp s2 sss
   | otherwise = solveProgram (newValue `union` mp) mask sss
   where
      newValue = fromList (zip (generateAddress (maskValue mask s1)) (repeat s2))

handleTotal :: Handle -> Handle -> [String] -> IO ()
handleTotal handleI handleO prevInput = do
    end <- hIsEOF handleI
    if end then
       hPutStr handleO $ pStr $ solveProgram empty "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" $ map parseProgram prevInput
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