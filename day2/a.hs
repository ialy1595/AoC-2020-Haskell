import System.IO( Handle, IOMode( ReadMode, WriteMode ), 
                  openFile, hGetLine, hClose, hIsEOF, hPutStr )

rInt :: String -> Int
rInt = read

pStr :: Int -> String
pStr = show

decryp :: String -> [String]
decryp [] = [""]
decryp (c:cs)
    | c == '-' = "" : rest
    | c == ' ' = "" : rest
    | c == ':' = rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = decryp cs

between :: Int -> Int -> Int -> Bool
between x y z = y <= x && x <= z

checkValid :: [String] -> Bool
checkValid (a:b:c:ds) = between (length $ filter (head c ==) (head ds)) (rInt a) (rInt b)

f :: [String] -> Int
f x = length $ filter checkValid $ map decryp x

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