import System.IO( Handle, IOMode( ReadMode, WriteMode ), 
                  openFile, hGetLine, hClose, hIsEOF, hPutStr )

import Data.Array( Array, array, range, listArray, bounds, assocs, (!), indices, elems )
import Data.Ix( Ix)
import Data.List( isPrefixOf )

rInt :: String -> Int
rInt = read

pStr :: Int -> String
pStr = show

arrEq :: (Ix i, Eq e) => Array i e -> Array i e -> Bool
arrEq x y = bounds x == bounds y && isPrefixOf (assocs x) (assocs y)

makeSeat :: [String] -> Array (Int, Int) Char
makeSeat ss = 
    let h = length ss
        w = length (head ss)
    in array ((1,1),(h, w)) [((x,y), (ss !! (x-1)) !! (y-1)) | x <- [1..h], y <- [1..w]]

nearRange :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
nearRange (h, w) (x, y) = range ((maximum [x - 1, 1], maximum [y - 1, 1]), (minimum [x + 1, h], minimum [y + 1, w]))
    
changeSeat :: Array (Int, Int) Char -> Array (Int, Int) Char
changeSeat arr = 
    let bd = snd (bounds arr)
        nearOccupied = length . filter (\x -> arr!x == '#') . nearRange bd
        f (i, e)
            | e == 'L' && nearOccupied i == 0 = '#'
            | e == '#' && nearOccupied i >= 5 = 'L'
            | otherwise = e
    in listArray (bounds arr) (map f (assocs arr))

simulSeat :: Array (Int, Int) Char -> Int
simulSeat arr
    | arrEq arr next = length (filter ('#' ==) (elems arr))
    | otherwise = simulSeat next
    where
        next = changeSeat arr

handleTotal :: Handle -> Handle -> [String] -> IO ()
handleTotal handleI handleO prevInput = do
    end <- hIsEOF handleI
    if end then
       hPutStr handleO $ pStr $ simulSeat (makeSeat prevInput)
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