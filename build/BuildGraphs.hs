module BuildGraphs (buildGraph,insertVert,insertNVerts,insertEdges,buildKn,buildKmn,buildCn,sum2Graphs,sumGraphs) where

import qualified Data.Map.Strict as Map
import Data.List as List

-- graf -> do wyrzucenia do osobnego pliku na koniec
type Graph = Map.Map Int [Int]


addEdge (a,b) = (Map.adjust (b:) a).(Map.adjust (a:) b)

insertions gr edges = List.foldr addEdge gr edges

correction gr = Map.map (nub.sort) gr


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
                else Map.fromList([(i,[j | j <- [1..m], j/=i]) | i <- [1..m]]++[(i,[j | j <- [m+1..m+n], j/=i]) | i <- [m+1..m+n]])

buildCn n = if n<=0 then Map.fromList []
            else if n==1 then Map.fromList [(1,[])]
            else if n==2 then Map.fromList [(1,[2]),(2,[1])]
            else Map.fromList(((1,[2,n]):[(i,[(i-1),(i+1)]) | i <- [2..n-1]])++[(n,[1,(n-1)])])

sum2Graphs g1 g2 = let  l1 = (Map.toList g1)
                        l2 = (Map.toList g2) 
                        n = (length l1) in
                    Map.fromList(l1++(map (\(a,b) -> (a+n,(map (+n) b))) l2))


sumGraphs [] = Map.fromList []
sumGraphs [g] = g
sumGraphs [g1,g2] = sum2Graphs g1 g2
sumGraphs (g:gs) = sum2Graphs g (sumGraphs gs)