import System.IO( Handle, IOMode( ReadMode, WriteMode ), 
                  openFile, hGetLine, hClose, hIsEOF, hPutStr )

import Data.Map( Map, fromList, (!))
import Data.List.Split( splitOn )


rInt :: String -> Int
rInt = read

pStr :: Int -> String
pStr = show

parseBagContain :: [String] -> [(Int,String)]
parseBagContain (s1:s2:s3:s4:ss)
    | s1 == "no" || s2 == "no" = []
    | s4 == "bags." || s4 == "bag." = [(rInt s1, s2 ++ " " ++ s3)]
    | s4 == "bags" || s4 == "bag" || s4 == "bags," || s4 == "bag," = (rInt s1, s2 ++ " " ++ s3) : parseBagContain ss
    | otherwise = parseBagContain (s2:s3:s4:ss)

parseBags :: [String] -> (String, [(Int, String)])
parseBags (s1:s2:ss) = (s1 ++" " ++ s2, parseBagContain ss)

parseInfo :: [String] -> Map String [(Int, String)]
parseInfo ss = fromList $ map (parseBags . splitOn " ") ss

calcBags :: Map String [(Int, String)] -> String -> Int
calcBags mp k = sum (map (\x -> fst x * calcBags mp (snd x)) (mp ! k)) + 1


f :: [String] -> Int
f ss = calcBags (parseInfo ss) "shiny gold" - 1

handleTotal :: Handle -> Handle -> [String] -> IO ()
handleTotal handleI handleO prevInput = do
    end <- hIsEOF handleI
    if end then
       hPutStr handleO $ pStr $ f prevInput
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