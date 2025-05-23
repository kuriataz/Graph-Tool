module GraphDegrees (vertexDegrees, degreeStats) where

import qualified Data.Map.Strict as Map

-- Reprezentacja grafu jako mapa sąsiedztwa
type Graph = Map.Map Int [Int]

-- Oblicza stopnie wszystkich wierzchołków
vertexDegrees :: Graph -> Map.Map Int Int
vertexDegrees graph = Map.map length graph

-- Oblicza minimalny i maksymalny stopień oraz histogram stopni
degreeStats :: Graph -> (Int, Int, Map.Map Int Int)
degreeStats graph =
  let degrees = Map.elems (vertexDegrees graph)
      maxDeg = maximum degrees
      minDeg = minimum degrees
      hist = Map.fromListWith (+) [(deg, 1) | deg <- degrees]
  in (maxDeg, minDeg, hist)