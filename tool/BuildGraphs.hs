module BuildGraphs (buildGraph,insertVert,insertNVerts,insertEdges,buildKn,buildKmn,buildCn,sum2Graphs,sumGraphs,buildEmptyGraph,buildPetersenGraph,buildGraphFromFile) where

import qualified Data.Map.Strict as Map
import Data.List as List
import Text.Read as Read

-- graf -> do wyrzucenia do osobnego pliku na koniec
type Graph = Map.Map Int [Int]

readAsInt :: String -> Int
readAsInt text = let mbRead = Read.readMaybe text in
                    case mbRead of
                        Just n -> n
                        Nothing -> 0    --will get auto-deleted later

getEdgesFromString [] = []
getEdgesFromString (e:edges) = let  vPair = (words e)++["0","0"]    --for security in case of incorrect data
                                    a = readAsInt (head vPair)
                                    b = readAsInt (head (tail vPair))
                                in (a,b):(getEdgesFromString edges)


addEdge (a,b) = (Map.adjust (b:) a).(Map.adjust (a:) b)

insertions gr edges = List.foldr addEdge gr edges

correction gr = Map.map (nub.sort) gr

liftIds gr m = if m<=0 then gr
                else Map.mapKeys (+m) (Map.map (map (+m)) gr)   -- adds m to all vertices ids and all values in adj. lists


-----------------------------------------------------------------------------------------

buildGraph n edges = correction (insertions (Map.fromList([(i,[]) | i <- [1..n]])) (filter (\(a,b) -> a/=b && a>0 && b>0 && a<=n && b<=n) edges))

insertVert gr = Map.insert ((Map.size gr)+1) [] gr

insertNVerts gr n = if n<1 then gr
                    else if n==1 then insertVert gr
                    else insertVert (insertNVerts gr (n-1))

insertEdges gr edges = let n=Map.size gr in
                        correction (insertions gr (filter (\(a,b) -> a/=b && a>0 && b>0 && a<=n && b<=n) edges))


-- constructors of special types of graphs


buildKn n = Map.fromList([(i,[j | j <- [1..n], j/=i]) | i <- [1..n]])

buildKmn m n = if m<=0 || n<0 then Map.fromList []
                else Map.fromList([(i,[j | j <- [m+1..m+n]]) | i <- [1..m]]++[(i,[j | j <- [1..m]]) | i <- [m+1..m+n]])

buildCn n = if n<=0 then Map.fromList []
            else if n==1 then Map.fromList [(1,[])]
            else if n==2 then Map.fromList [(1,[2]),(2,[1])]
            else Map.fromList(((1,[2,n]):[(i,[(i-1),(i+1)]) | i <- [2..n-1]])++[(n,[1,(n-1)])])


sum2Graphs isStrict g1 g2 = let n=Map.size g1
                                m=Map.size g2
                            in if isStrict then Map.union g1 (liftIds g2 n) --strict order of graph parts
                                else if(n<=m) then Map.union g2 (liftIds g1 m)    -- updating ids in smaller graph is often faster (unless it has much more edges)
                                    else Map.union g1 (liftIds g2 n)            -- Map.union is faster when the 1st object is bigger


sumGraphs _ [] = Map.fromList []
sumGraphs isStrict graphs = foldr (sum2Graphs isStrict) (Map.fromList []) graphs



buildEmptyGraph = buildKn 0

buildPetersenGraph = insertEdges (sum2Graphs True (buildCn 5) (buildCn 5)) [(1,6),(2,9),(3,7),(4,10),(5,8)]


buildGraphFromFile filePath = do
    content <- readFile(filePath)
    let linesList = lines content
        n = readAsInt (head linesList)
        edges = getEdgesFromString (tail linesList)
        gr = buildGraph n edges
    return gr