module IOInteractions(printGraph,buildGraphFromFile,toIO) where

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


-------------------------------------------------------------

--na razie to jedno nie działa
printGraph gr = putStr (Map.foldrWithKey (\key val str -> str++show(key)++": "++show(val)++"\n") "" gr)

buildGraphFromFile :: String -> IO(Graph)
buildGraphFromFile filePath = do
    content <- readFile(filePath)
    let linesList = lines content
        n = readAsInt (head linesList)
        edges = getEdgesFromString (tail linesList)
        gr = buildGraph n edges
    return gr



toIO func ioGraph = do
    gr <- ioGraph
    let result = func gr
    return result