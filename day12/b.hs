import System.IO( Handle, IOMode( ReadMode, WriteMode ), 
                  openFile, hGetLine, hClose, hIsEOF, hPutStr )

rInt :: String -> Int
rInt = read

pStr :: Int -> String
pStr = show

parseNavi :: String -> (Char, Int)
parseNavi (c:cs) = (c, rInt cs)

rotateDir :: (Int, Int) -> Int -> (Int, Int)
rotateDir (x, y) r = [(x, y), (-y, x), (-x, -y), (y, -x)] !! ((r `div` 90) `mod` 4)

moveStorm :: Int -> Int -> (Int, Int) -> [(Char, Int)] -> Int
moveStorm x y _ [] = abs x + abs y
moveStorm x y (dx, dy) ((c, a):cas)
    | c == 'N' = moveStorm x y (dx, dy + a) cas
    | c == 'S' = moveStorm x y (dx, dy - a) cas
    | c == 'E' = moveStorm x y (dx + a, dy) cas
    | c == 'W' = moveStorm x y (dx - a, dy) cas
    | c == 'L' = moveStorm x y (rotateDir (dx, dy) a) cas
    | c == 'R' = moveStorm x y (rotateDir (dx, dy) (negate a)) cas
    | otherwise = moveStorm (x + a * dx) (y + a * dy) (dx, dy) cas
    
handleTotal :: Handle -> Handle -> [String] -> IO ()
handleTotal handleI handleO prevInput = do
    end <- hIsEOF handleI
    if end then
       hPutStr handleO $ pStr $ moveStorm 0 0 (10, 1) (map parseNavi prevInput)
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