import System.IO( Handle, IOMode( ReadMode, WriteMode ), 
                  openFile, hGetLine, hClose, hIsEOF, hPutStr )

import Data.Map( Map, fromList, (!))
import Data.Graph.Inductive.Graph(Node, LNode, LEdge, mkGraph )
import Data.Graph.Inductive.PatriciaTree( Gr )
import Data.Graph.Inductive.Query.BFS( bfs )
import Data.List.Split( splitOn )


rInt :: String -> Int
rInt = read

pStr :: Int -> String
pStr = show

parseBags :: [String] -> [String]
parseBags (s1:s2:s3:ss)
    | s1 == "no" = []
    | s3 == "bags." || s3 == "bag." = [s1 ++ " " ++ s2]
    | s3 == "bags" || s3 == "bag" || s3 == "bags," || s3 == "bag," = (s1 ++ " " ++ s2) : parseBags ss
    | otherwise = parseBags (s2:s3:ss)

parseInfo :: [String] -> [[String]]
parseInfo = map (parseBags . splitOn " ")

nodeList :: [[String]] -> [LNode String]
nodeList sss = zip [0..] (map head sss)

getNodeMap :: [[String]] -> Map String Int
getNodeMap sss = fromList $ zip (map head sss) [0..]

parseEdge :: Map String Int -> [String] -> [LEdge Int]
parseEdge mp (s1:s2:ss) = (mp ! s2, mp ! s1, 1) : parseEdge mp (s1:ss)
parseEdge _ _ = []

edgeList :: [[String]] -> [LEdge Int]
edgeList sss = concatMap (parseEdge mp) sss
    where
        mp = getNodeMap sss

getGraph :: [[String]] -> Gr String Int
getGraph sss = mkGraph nodes edges
    where
        nodes = nodeList sss
        edges = edgeList sss

doBfs :: [[String]] -> Int
doBfs sss = length (bfs st gr) - 1
    where
        st = getNodeMap sss ! "shiny gold"
        gr = getGraph sss

handleTotal :: Handle -> Handle -> [String] -> IO ()
handleTotal handleI handleO prevInput = do
    end <- hIsEOF handleI
    if end then
       hPutStr handleO $ pStr $ doBfs $ parseInfo prevInput
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