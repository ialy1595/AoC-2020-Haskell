import System.IO( Handle, IOMode( ReadMode, WriteMode ), 
                  openFile, hGetLine, hClose, hIsEOF, hPutStr )

rInt :: String -> Int
rInt = read

pStr :: Int -> String
pStr = show

treeCount :: Char -> Int
treeCount '#' = 1
treeCount _ = 0

f :: [String] -> Int -> (Int, Int) -> Int
f [] _ (_,_) = 0
f (x:xs) i (w,h)
    | i `mod` h == 0 = treeCount (x !! ((i * w `div` h) `mod` length x)) + f xs (succ i) (w,h)
    | otherwise = f xs (succ i) (w,h)

handleTotal :: Handle -> Handle -> [String] -> IO ()
handleTotal handleI handleO prevInput = do
    end <- hIsEOF handleI
    if end then
       hPutStr handleO $ pStr $ foldr ((*) . f prevInput 0) 1 [(1,1), (3,1), (5,1), (7,1), (1,2)]
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