-- | Module for graph analysis, providing functions to compute vertex degrees,
-- find connected components, compute component diameters, and distance statistics.
module GraphAnalysis
  ( Graph
  , vertexDegrees
  , degreeStats
  , findComponents
  , componentDiameters
  , distanceStats
  , buildGraph
  , insertVert
  , insertNVerts
  , insertEdges
  , buildKn
  , buildKmn
  , buildCn
  , sum2Graphs
  , sumGraphs
  , buildEmptyGraph
  , buildPetersenGraph
  , buildGraphFromFile
  , toIO
  , printGraph
  , printIOGraph
  , writeGraphToFile
  , writeIOGraphToFile
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Maybe (fromMaybe)
import Data.List (foldl')
import BuildGraphs
import IOInteractions

-- | Graph representation as an adjacency map, where the key is a vertex
-- and the value is a list of its neighbors.

-- | Computes degrees of all vertices
-- Returns a map where key = vertex value = degree.
vertexDegrees :: Graph -> Map.Map Int Int
vertexDegrees = Map.map length

-- | Computes degree stats (max, min, histogram).
degreeStats :: Graph -> (Int, Int, Map.Map Int Int)
degreeStats graph =
  let degrees = Map.elems (vertexDegrees graph)
      maxDeg = maximum degrees
      minDeg = minimum degrees
      hist = Map.fromListWith (+) [(deg, 1) | deg <- degrees]
  in (maxDeg, minDeg, hist)

-- | Finds all connected components.
-- Returns a list of list of vertices (every inner list is one CC).
findComponents :: Graph -> [[Int]]
findComponents graph = go (Set.fromList (Map.keys graph)) []
  where
    go unvisited components
      | Set.null unvisited = components
      | otherwise =
          let start = Set.findMin unvisited
              component = dfs start Set.empty
              newUnvisited = Set.difference unvisited component
          in go newUnvisited (Set.toList component : components)

    dfs v visited
      | Set.member v visited = visited
      | otherwise =
          let neighbors = fromMaybe [] (Map.lookup v graph)
              visited' = Set.insert v visited
          in foldl' (flip dfs) visited' neighbors

-- | Computes the diameter of every CC.
-- Returns a list of pairs (CC, diameter),
-- diameter of CC = the longest shortest path in the CC.
componentDiameters :: Graph -> [([Int], Int)]
componentDiameters graph =
  let components = findComponents graph
  in map (\comp -> (comp, diameter graph comp)) components

-- | Computes the diameter of one CC using BFS.
diameter :: Graph -> [Int] -> Int
diameter graph component =
  maximum [maxDistance graph v component | v <- component]

-- | Finds max. distance from start to other vertices in one CC using BFS.
maxDistance :: Graph -> Int -> [Int] -> Int
maxDistance graph start component =
  let compSet = Set.fromList component
      distances = bfs graph start compSet
  in maximum (Map.elems distances)

-- | Breadth First Search.
-- Returns a map of distances to all vertices in given CC.
bfs :: Graph -> Int -> Set.Set Int -> Map.Map Int Int
bfs graph start compSet =
  let queue = Seq.singleton start
      distances = Map.singleton start 0
      visited = Set.singleton start
  in bfs' graph compSet queue distances visited
  where
    bfs' graph compSet queue distances visited
      | Seq.null queue = distances
      | otherwise =
          let (v Seq.:< queue') = Seq.viewl queue
              neighbors = fromMaybe [] (Map.lookup v graph)
              distV = fromMaybe 0 (Map.lookup v distances)
              (newQueue, newDistances, newVisited) =
                foldl'
                  (\(q, d, vis) u ->
                    if Set.member u compSet && not (Set.member u vis)
                      then
                        ( q Seq.|> u
                        , Map.insert u (distV + 1) d
                        , Set.insert u vis
                        )
                      else (q, d, vis))
                  (queue', distances, visited)
                  neighbors
          in bfs' graph compSet newQueue newDistances newVisited

-- | Computes distance stats (histogram of distance between every pair in CC).
-- Returns a list of pairs (CC, histogram).
distanceStats :: Graph -> [([Int], Map.Map Int Int)]
distanceStats graph =
  let components = findComponents graph
  in map (\comp -> (comp, distanceHistogram graph comp)) components

-- | Computes histogram for a given CC.
distanceHistogram :: Graph -> [Int] -> Map.Map Int Int
distanceHistogram graph component =
  let compSet = Set.fromList component
      allDistances =
        [ dist
        | v <- component
        , let distances = bfs graph v compSet
        , dist <- Map.elems distances
        , dist > 0
        ]
  in Map.fromListWith (+) [(dist, 1) | dist <- allDistances]