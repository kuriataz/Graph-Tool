module IOInteractions(buildGraphFromFile,printGraph,printIOGraph,toIO,writeGraphToFile,writeIOGraphToFile) where

import qualified Data.Map.Strict as Map
import Data.List as List
import Text.Read as Read
import BuildGraphs(Graph,buildGraph,vertCount)

-- helping functions

-- converts String to Int; if the String can't be read as Int, returns 0 (all 0s are deleted anyway when building a graph)
-- uses built-in function Read.readMaybe 
readAsInt :: String -> Int
readAsInt text = let mbRead = Read.readMaybe text in
                    case mbRead of
                        Just n -> n
                        Nothing -> 0    --will get auto-deleted later

-- converts list of Strings to list of egdes (pairs of Ints)
-- uses function readAsInt and built-in functions words, head and tail
getEdgesFromString :: [String] -> [(Int,Int)]
getEdgesFromString [] = []
getEdgesFromString (e:edges) = let  vPair = (words e)++["0","0"]    --for security in case of incorrect data
                                    a = readAsInt (head vPair)
                                    b = readAsInt (head (tail vPair))
                                in (a,b):(getEdgesFromString edges)


-- converts a graph to String meant for printing
-- uses built-in functions Map.foldrWithKey and show
toPrintStr :: Graph -> String
toPrintStr gr = Map.foldrWithKey (\key val str -> show(key)++": "++show(val)++"\n"++str) "" gr


-- converts adjacency list of a vertex to edges list
adjListToEdges :: (Int,[Int]) -> [(Int,Int)]
adjListToEdges (vert,adjVerts) = [(vert,neigh) | neigh <- adjVerts]

-- converts a graph to the list of its edges
-- uses function adjListToEdges and built-in functions foldl, map and Map.toList
graphToEdgeList :: Graph -> [(Int,Int)]
graphToEdgeList gr = foldl (++) [] (map adjListToEdges (Map.toList gr))

-- converts list of edges to its String representation meant for saving in a file
-- uses built-in functions foldl, map and show
edgeListToString :: [(Int,Int)] -> String
edgeListToString edges = foldl (++) "" (map (\(a,b) -> (show a)++" "++(show b)++"\n") edges)

-- converts a graph to String meant for saving in a file
-- uses functions edgeListToString, graphToEdgeList and vertCount from module BuildGraphs
graphToFileString :: Graph -> String
graphToFileString gr = (show (vertCount gr))++"\n"++(edgeListToString (graphToEdgeList gr))

-------------------------------------------------------------

-- reads content of a file and builds a graph from it
-- the result is of type IO(Graph)
-- uses functions readAsInt, getEdgesFromString, function buildGraph from module BuildGraphs and built-in functions readFile, lines, head and tail
buildGraphFromFile :: String -> IO(Graph)
buildGraphFromFile filePath = do
    content <- readFile(filePath)
    let linesList = lines content
        n = readAsInt (head linesList)
        edges = getEdgesFromString (tail linesList)
        gr = buildGraph n edges
    return gr


-- prints the graph in a read-friendly form
-- uses function toPrintStr and built-in function putStr
printGraph :: Graph -> IO()
printGraph gr = putStr (toPrintStr gr)

-- version of printGraph for graphs within the IO monad
-- uses function printGraph
printIOGraph :: IO(Graph) -> IO()
printIOGraph ioGraph = do
    gr <- ioGraph
    printGraph gr


-- writes the graph's data to a text file in the format allowing the result to be read again
-- uses function graphToFileString and built-in function writeFile
writeGraphToFile :: String -> Graph -> IO()
writeGraphToFile filePath gr = writeFile filePath (graphToFileString gr)

-- version of writeGraphToFile for graphs within the IO monad
-- uses function writeGraphToFile
writeIOGraphToFile :: String -> IO(Graph) -> IO()
writeIOGraphToFile filePath ioGraph = do
    gr <- ioGraph
    writeGraphToFile filePath gr



-- function allowing calling one-argument functions on graph within the IO monad
toIO :: (a -> b) -> IO(a) -> IO(b)
toIO func ioGraph = do
    gr <- ioGraph
    let result = func gr
    return result