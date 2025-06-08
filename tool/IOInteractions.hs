module IOInteractions(buildGraphFromFile,printGraph,printIOGraph,toIO,writeGraphToFile,writeIOGraphToFile) where

import qualified Data.Map.Strict as Map
import Data.List as List
import Text.Read as Read
import BuildGraphs(Graph,buildGraph)

-- helping functions

readAsInt :: String -> Int
readAsInt text = let mbRead = Read.readMaybe text in
                    case mbRead of
                        Just n -> n
                        Nothing -> 0    --will get auto-deleted later

getEdgesFromString :: [String] -> [(Int,Int)]
getEdgesFromString [] = []
getEdgesFromString (e:edges) = let  vPair = (words e)++["0","0"]    --for security in case of incorrect data
                                    a = readAsInt (head vPair)
                                    b = readAsInt (head (tail vPair))
                                in (a,b):(getEdgesFromString edges)


toPrintStr :: Graph -> String
toPrintStr gr = Map.foldrWithKey (\key val str -> show(key)++": "++show(val)++"\n"++str) "" gr


adjListToEdges :: (Int,[Int]) -> [(Int,Int)]
adjListToEdges (vert,adjVerts) = [(vert,neigh) | neigh <- adjVerts]

graphToEdgeList :: Graph -> [(Int,Int)]
graphToEdgeList gr = foldl (++) [] (map adjListToEdges (Map.toList gr))

edgeListToString :: [(Int,Int)] -> String
edgeListToString edges = foldl (++) "" (map (\(a,b) -> (show a)++" "++(show b)++"\n") edges)

graphToFileString :: Graph -> String
graphToFileString gr = edgeListToString (graphToEdgeList gr)

-------------------------------------------------------------

buildGraphFromFile :: String -> IO(Graph)
buildGraphFromFile filePath = do
    content <- readFile(filePath)
    let linesList = lines content
        n = readAsInt (head linesList)
        edges = getEdgesFromString (tail linesList)
        gr = buildGraph n edges
    return gr


printGraph :: Graph -> IO()
printGraph gr = putStr (toPrintStr gr)

printIOGraph :: IO(Graph) -> IO()
printIOGraph ioGraph = do
    strGr <- toIO toPrintStr ioGraph
    putStr strGr


writeGraphToFile :: String -> Graph -> IO()
writeGraphToFile filePath gr = writeFile filePath (graphToFileString gr)

writeIOGraphToFile :: String -> IO(Graph) -> IO()
writeIOGraphToFile filePath ioGraph = do
    gr <- ioGraph
    writeFile filePath (graphToFileString gr)



toIO :: (a -> b) -> IO(a) -> IO(b)
toIO func ioGraph = do
    gr <- ioGraph
    let result = func gr
    return result