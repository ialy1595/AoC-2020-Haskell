import System.IO( Handle, IOMode( ReadMode, WriteMode ), 
                  openFile, hGetLine, hClose, hIsEOF, hPutStr )

import Data.List.Split( splitOn )
import Data.List( isPrefixOf )
import Data.Map (Map, elems, insert, empty, member, (!) , fromList)

rInt :: String -> Int
rInt = read

pStr :: Int -> String
pStr = show

parseRange :: String -> (Int, Int)
parseRange s =
   let nums = splitOn "-" s
   in (rInt (head nums), rInt (last nums))

parseField :: String -> [(Int, Int)]
parseField = map parseRange . splitOn " or " . last . splitOn ": "


parseTicket :: [String] -> ([[(Int, Int)]], [Int], [[Int]])
parseTicket ss =
   let part = splitOn [""] ss
   in (map parseField (head part) , (map rInt . splitOn "," . last) (part !! 1), (map (map rInt . splitOn ",") . tail) (last part))

between :: Int -> Int -> Int -> Bool
between x y z = y <= x && x <= z

validField :: Int -> [(Int, Int)] -> Bool
validField x = any (uncurry (between x))

findValidTickets :: [[(Int, Int)]] -> [[Int]] -> [[Int]]
findValidTickets fields nearTickets =
   let invalidField x fd = not (validField x fd)
       isInvalid x = all (invalidField x) fields
       checkValid = (not . any isInvalid)
   in filter checkValid nearTickets

printIntList :: [Int] -> String
printIntList [] = ""
printIntList (i:is) = pStr i ++ " " ++ printIntList is

printISList :: [(Int, String)] -> String
printISList [] = "" 
printISList (i:is) = pStr (fst i) ++ ": " ++ snd i ++ "\n" ++ printISList is

handleTotal :: Handle -> Handle -> [String] -> IO ()
handleTotal handleI handleO prevInput = do
    end <- hIsEOF handleI
    if end then
       let (fields, myTicket, nearTickets) = parseTicket prevInput
           validTickets = findValidTickets fields nearTickets
           departureFields = fields
           satisfyField idx fd = all ((`validField` fd) . (!! idx)) (myTicket:validTickets)
           findField idx = if any (satisfyField idx) departureFields then (idx, fi) else (idx, "none")
              where fi = printIntList (filter (\x -> satisfyField idx (departureFields !! x)) [0..(length departureFields - 1)])
       in hPutStr handleO $ printISList $ map findField [0..(length myTicket - 1)]
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