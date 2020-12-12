import System.IO( Handle, IOMode( ReadMode, WriteMode ), 
                  openFile, hGetLine, hClose, hIsEOF, hPutStr )

rInt :: String -> Int
rInt = read

pStr :: Int -> String
pStr = show

parseNavi :: String -> (Char, Int)
parseNavi (c:cs) = (c, rInt cs)

moveStorm :: Int -> Int -> Int -> [(Char, Int)] -> Int
moveStorm x y _ [] = abs x + abs y
moveStorm x y d ((c, a):cas)
    | c == 'N' = moveStorm x (y + a) d cas
    | c == 'S' = moveStorm x (y - a) d cas
    | c == 'E' = moveStorm (x + a) y d cas
    | c == 'W' = moveStorm (x - a) y d cas
    | c == 'L' = moveStorm x y ((d + 1 * (a `div` 90)) `mod` 4) cas
    | c == 'R' = moveStorm x y ((d + 3 * (a `div` 90)) `mod` 4) cas
    | otherwise = moveStorm (x + a * fst (dir d)) (y + a * snd (dir d)) d cas
    where
        dir = ([(1, 0), (0, 1), (-1, 0), (0, -1)] !!)
    
handleTotal :: Handle -> Handle -> [String] -> IO ()
handleTotal handleI handleO prevInput = do
    end <- hIsEOF handleI
    if end then
       hPutStr handleO $ pStr $ moveStorm 0 0 0 (map parseNavi prevInput)
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