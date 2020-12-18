import System.IO( Handle, IOMode( ReadMode, WriteMode ), 
                  openFile, hGetLine, hClose, hIsEOF, hPutStr )

import Data.Sequence( Seq, fromList, length, index, (<|), (|>), (><), drop, reverse, elemIndexR, take, empty )
import Data.List.Split( splitOn )
import Data.Maybe( fromMaybe )

rInt :: String -> Int
rInt = read

pStr :: Int -> String
pStr = show

seqLen :: Seq a -> Int
seqLen = Data.Sequence.length

seqDrop :: Int -> Seq a -> Seq a
seqDrop = Data.Sequence.drop

seqTake :: Int -> Seq a -> Seq a
seqTake = Data.Sequence.take

seqRev :: Seq a -> Seq a
seqRev = Data.Sequence.reverse

seqFindR :: Eq a => a -> Seq a -> Int
seqFindR x = fromMaybe (-1) . elemIndexR x

seqHead :: Seq a -> a
seqHead s = s `index` 0

seqTail :: Seq a -> Seq a
seqTail = seqDrop 1

spacePar :: String -> String
spacePar "" = ""
spacePar (c:cs)
   | c == '(' = "( " ++ rest
   | c == ')' = " )" ++ rest
   | otherwise  = c:rest
   where
      rest = spacePar cs

parseExpr :: String -> Seq String
parseExpr = fromList . splitOn " " . spacePar

calcExprNoPar :: Seq String -> Seq String
calcExprNoPar ss
   | seqLen ss == 1 = ss
   | ss `index` 1 == "+" = calcExprNoPar (pStr (getIntOf 0 + getIntOf 2) <| seqDrop 3 ss)
   | otherwise = seqTake 2 ss >< calcExprNoPar (seqDrop 2 ss)
   where
      getIntOf = rInt . index ss

calcExprNoAdd :: Seq String -> Int
calcExprNoAdd ss
   | seqLen ss == 1 = getIntOf 0
   | otherwise = calcExprNoAdd (pStr (getIntOf 0 * getIntOf 2) <| seqDrop 3 ss)
   where
      getIntOf = rInt . index ss


calcExpr :: Seq String -> Seq String -> Int
calcExpr s1 s2
   | s2 == empty = (calcExprNoAdd . calcExprNoPar) s1
   | s == ")" = calcExpr (seqTake lpIdx s1 |> (pStr . calcExprNoAdd . calcExprNoPar . seqDrop (lpIdx + 1)) s1) ss
   | otherwise = calcExpr (s1 |> s) ss
   where
      s = seqHead s2
      ss = seqTail s2
      lpIdx = seqFindR "(" s1

handleTotal :: Handle -> Handle -> [String] -> IO ()
handleTotal handleI handleO prevInput = do
    end <- hIsEOF handleI
    if end then
       hPutStr handleO $ pStr $ sum $ map (calcExpr empty . parseExpr) prevInput
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