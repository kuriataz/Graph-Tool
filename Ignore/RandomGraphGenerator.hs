module RandomGraphGenerator() where

import BuildGraphs(Graph,buildGraph)
import qualified System.Random as Rand


randomEdges :: Int -> Int -> Int -> IO([(Int,Int)])
randomEdges 0 _ _ = []
randomEdges n from to = (((Rand.randomRIO from to),(Rand.randomRIO from to))):(randomEdges (n-1) from to)


randomGraph :: Int -> IO(Graph)
randomGraph n = do
    numberOfRandomEdges <- Rand.randomRIO 0 (div (n*(n-1)) 2)
    edges <- randomEdges numberOfRandomEdges 1 n
    let
        gr = buildGraph n edges
    return gr