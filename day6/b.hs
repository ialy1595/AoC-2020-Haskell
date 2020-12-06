import System.IO( Handle, IOMode( ReadMode, WriteMode ), 
                  openFile, hGetLine, hClose, hIsEOF, hPutStr )

rInt :: String -> Int
rInt = read

pStr :: Int -> String
pStr = show

splitGroup :: [String] -> [[String]]
splitGroup [] = [[]]
splitGroup (c:cs)
    | c == "" = [] : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = splitGroup cs

allElem :: [String] -> Char -> Bool
allElem ss c = all (elem c) ss

findCommon :: [String] -> String
findCommon ss = filter (allElem ss) "abcdefghijklmnopqrstuvwxyz"

handleTotal :: Handle -> Handle -> [String] -> IO ()
handleTotal handleI handleO prevInput = do
    end <- hIsEOF handleI
    if end then
       hPutStr handleO $ pStr $ sum $ map (length . findCommon) (splitGroup prevInput)
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