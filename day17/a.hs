import System.IO( Handle, IOMode( ReadMode, WriteMode ), 
                  openFile, hGetLine, hClose, hIsEOF, hPutStr )

import Data.List( transpose )
import Data.Array.Diff( DiffArray, array, range, listArray, bounds, assocs, (!), indices, elems, (//), ixmap )

rInt :: String -> Int
rInt = read

pStr :: Int -> String
pStr = show

makeArr :: [String] -> DiffArray (Int, Int, Int, Int) Char
makeArr ss = 
   let xl = length ss
       yl = length (head ss)
       zl = maximum [xl, yl]
       wl = zl
       base = array ((0,0,0,0), (xl+14, yl+14, zl+14, wl+14)) [(i, '.') | i <- range ((0,0,0,0), (xl+14, yl+14, zl+14, wl+14))]
   in base // [((x + 7,y + 7,7,7), (ss !! x) !! y) | x <- [0..(xl - 1)], y <- [0..(yl - 1)]]

changeRule :: Char -> Int -> Char
changeRule '#' 4 = '#'
changeRule _ 3 = '#'
changeRule _ _ = '.'

mergeElem :: [((Int, Int, Int, Int), Char)] -> ((Int, Int, Int, Int), Char)
mergeElem ics = ((fst . head) ics, changeRule (snd (ics !! 40)) ((length . filter (=='#') . map snd) ics))

simulArr :: DiffArray (Int, Int, Int, Int) Char -> DiffArray (Int, Int, Int, Int) Char
simulArr arr = 
   let (xl, yl, zl, wl) = (snd . bounds) arr
       newBase = array ((0,0,0,0), (xl, yl, zl, wl)) [(i, '.') | i <- range ((0,0,0,0), (xl, yl, zl, wl))]
       moveMap (dx,dy,dz,dw) = ixmap ((1,1,1,1), (xl - 1, yl - 1, zl - 1, wl - 1)) (\(x,y,z,w) -> (x+dx,y+dy,z+dz,w+dw)) arr
       neighbors = map (assocs  . moveMap) (range ((-1,-1,-1,-1), (1,1,1,1)))
   in newBase // (map mergeElem . transpose) neighbors

f :: DiffArray (Int, Int, Int, Int) Char -> Int -> Int
f arr 6 = (length . filter (=='#') . elems) arr
f arr x = f (simulArr arr) (x + 1)

handleTotal :: Handle -> Handle -> [String] -> IO ()
handleTotal handleI handleO prevInput = do
    end <- hIsEOF handleI
    if end then
       hPutStr handleO $ pStr $ f (makeArr prevInput) 0
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