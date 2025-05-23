module GraphComponents (findComponents) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.List (foldl')

type Graph = Map.Map Int [Int]

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
          in foldl' (\acc u -> dfs u acc) visited' neighbors