module BuildGraphs (Graph,isEmptyGraph,vertCount,edgeCount,buildGraph,insertVert,insertNVerts,insertEdges,buildKn,buildKmn,buildCn,sum2Graphs,sumGraphs,buildEmptyGraph,buildPetersenGraph) where

import qualified Data.Map.Strict as Map
import Data.List as List
import Text.Read as Read

type Graph = Map.Map Int [Int]

-- helping functions; used only by other functions and not shared outside the module


-- inserts edges from the list to the graph (vertices values validated by the calling function)
-- uses built-in functions Map.adjust and List.foldl'
insertions :: Graph -> [(Int,Int)] -> Graph
insertions gr edges = List.foldl' (\g (a,b) -> (Map.adjust (b:) a (Map.adjust (a:) b g))) gr edges

-- sorts all the adjacency lists of the graph and removes dupliacted values
-- uses bulit-in functions nub and sort
correction :: Graph -> Graph
correction gr = Map.map (nub.sort) gr

-- adds m to all the vertices ids (including ids from the adjacency lists)
-- uses built-in functions Map.mapKeys, Map.map and map
liftIds :: Graph -> Int -> Graph
liftIds gr m = if m<=0 then gr
                else Map.mapKeys (+m) (Map.map (map (+m)) gr)   -- adds m to all vertices ids and all values in adj. lists


-----------------------------------------------------------------------------------------

-- returns information if the provided graph is empty (0 vertices)
-- uses built-in function Map.null
isEmptyGraph :: Graph -> Bool
isEmptyGraph gr = Map.null gr

-- returns number of vertices in the graph
-- uses built-in function Map.size
vertCount :: Graph -> Int
vertCount gr = Map.size gr

-- returns number of edges in the graph
-- uses built-in functions Map.foldl, div and length
edgeCount :: Graph -> Int
edgeCount gr = div (Map.foldl (\acc list -> acc + (length list)) 0 gr) 2



-- builds a graph with n vertices (ids 1..n) and edges from the list
-- if n<=0, the graph is empty
-- removes all edges containing invalid vertices (id <=0 or >n) and looping edges
-- uses functions correcton and insertion from the "helping functions" section and built-in functions Map.fromList and filter
buildGraph :: Int -> [(Int,Int)] -> Graph
buildGraph n edges = correction (insertions (Map.fromList([(i,[]) | i <- [1..n]])) (filter (\(a,b) -> a/=b && a>0 && b>0 && a<=n && b<=n) edges))

-- adds a new vertex (with the first free id) to the graph; doesn't add any edges, so the new vertex is isolated
-- uses function vertCount and built-in function Map.insert
insertVert :: Graph -> Graph
insertVert gr = Map.insert ((vertCount gr)+1) [] gr

-- adds n new vertices (with the first n free ids) to the graph; doesn't add any edges, so the new vertices are isolated
-- if n<=0, returns the original graph
-- uses function insertVert
insertNVerts :: Graph -> Int -> Graph
insertNVerts gr n = if n<1 then gr
                    else if n==1 then insertVert gr
                    else insertVert (insertNVerts gr (n-1))

-- adds the edges from the list to the graph
-- removes all edges containing invalid vertices (id <=0 or >maxVertId) and looping edges
-- uses functions correcton and insertion from the "helping functions" section, function vertCount and built-in function filter
insertEdges :: Graph -> [(Int,Int)] -> Graph
insertEdges gr edges = let n=vertCount gr in
                        correction (insertions gr (filter (\(a,b) -> a/=b && a>0 && b>0 && a<=n && b<=n) edges))


-- constructors of special types of graphs

-- builds a graph of type Kn with specified number of vertices
-- uses built-in function Map.fromList
buildKn :: Int -> Graph
buildKn n = Map.fromList([(i,[j | j <- [1..n], j/=i]) | i <- [1..n]])

-- builds a graph of type Km,n with specified number of vertices in each of the groups
-- if any of the numbers provided is negative, returns an empty graph
-- uses built-in function Map.fromList
buildKmn :: Int -> Int -> Graph
buildKmn m n = if m<0 || n<0 then Map.fromList []
                else Map.fromList([(i,[j | j <- [m+1..m+n]]) | i <- [1..m]]++[(i,[j | j <- [1..m]]) | i <- [m+1..m+n]])

-- builds a cycle with specified number of vertices
-- uses built-in function Map.fromList
buildCn :: Int -> Graph
buildCn n = if n<=0 then Map.fromList []
            else if n==1 then Map.fromList [(1,[])]
            else if n==2 then Map.fromList [(1,[2]),(2,[1])]
            else Map.fromList(((1,[2,n]):[(i,[(i-1),(i+1)]) | i <- [2..n-1]])++[(n,[1,(n-1)])])


-- creates one graph out of two graphs provided; doesn't add any new edges
-- if isStrict is True, the ids will be changed (to avoid duplicates) in the way keeping the original order of the graphs
-- if isStrict is False, the ids will be changed (to avoid duplicates) in the order the will cause the function to work faster
-- uses function liftIds from the "helping functions" section, function vertCount and built-in function Map.union
sum2Graphs :: Bool -> Graph -> Graph -> Graph
sum2Graphs isStrict g1 g2 = let n=vertCount g1
                                m=vertCount g2
                            in if isStrict then Map.union g1 (liftIds g2 n)     --strict order of graph parts
                                else if(n<=m) then Map.union g2 (liftIds g1 m)  -- updating ids in smaller graph is often faster (unless it has much more edges)
                                    else Map.union g1 (liftIds g2 n)            -- Map.union is faster when the 1st object is bigger


-- creates one graph out of a graph list provided; doesn't add any new edges
-- if isStrict is True, the ids will be changed (to avoid duplicates) in the way keeping the original order of the graphs
-- if isStrict is False, the ids will be changed (to avoid duplicates) in the order the will cause the function to work faster
-- uses function sum2Graphs and built-in functions Map.fromList and foldr
sumGraphs :: Bool -> [Graph] -> Graph
sumGraphs _ [] = Map.fromList []
sumGraphs isStrict graphs = foldr (sum2Graphs isStrict) (Map.fromList []) graphs


-- creates an empty graph (0 vertices and 0 edges)
-- uses function buildKn
buildEmptyGraph :: Graph
buildEmptyGraph = buildKn 0

-- creates the Petersen graph
-- uses functions insertEdges, sum2Graphs and buildCn
buildPetersenGraph :: Graph
buildPetersenGraph = insertEdges (sum2Graphs True (buildCn 5) (buildCn 5)) [(1,6),(2,9),(3,7),(4,10),(5,8)]