import System.IO( Handle, IOMode( ReadMode, WriteMode ), 
                  openFile, hGetLine, hClose, hIsEOF, hPutStr )

rInt :: String -> Int
rInt = read

pStr :: Int -> String
pStr = show

cannotMake :: [Int] -> Int -> Bool
cannotMake as a = null ([(x, y) | x <- as, y <- as, x /= y, x + y == a])

invVal :: Int -> [Int] -> Int
invVal idx xd
    | cannotMake prevNum value = value
    | otherwise = invVal (idx + 1) xd
    where
        value = xd !! idx
        prevNum = take 25 (drop (idx - 25) xd)

subList :: [Int] -> Int -> Int -> [Int]
subList as s e = drop s (take (e + 1) as)

minMax :: [Int] -> Int
minMax as = minimum as + maximum as

findSum :: [Int] -> Int -> Int -> Int -> Int
findSum as x s e
    | sumSE < x = findSum as x s (e + 1)
    | sumSE > x = findSum as x (s + 1) e
    | otherwise = res
    where
        sumSE = sum (subList as s e)
        res = minMax (subList as s e)
    

f :: [Int] -> Int
f as = findSum as inv 0 0
    where
        inv = invVal 25 as

handleTotal :: Handle -> Handle -> [String] -> IO ()
handleTotal handleI handleO prevInput = do
    end <- hIsEOF handleI
    if end then
       hPutStr handleO $ pStr $ f $ map rInt prevInput
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