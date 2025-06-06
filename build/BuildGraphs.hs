-- na razie tak, żeby było prościej pracować nad resztą rzeczy; na koniec można poszukać szybszej wersji (ta zbyt wymagająca dla dużych grafów gęstych)

module BuildGraphs (buildGraphStandard,buildGraphLight,buildKn,buildKmn,buildCn,sum2Graphs,sumGraphs) where

import qualified Data.Map.Strict as Map
import Data.List

-- graf -> do wyrzucenia do osobnego pliku na koniec
type Graph = Map.Map Int [Int]

-- helping functions; they're used only by other functions which check whether the arguments are correct
addReverses edges = edges++(map (\(e,f) -> (f,e)) edges)

partition2 _ _ [] = ([],[],[])
partition2 cond1 cond2 (x:xs) = if cond1 x then (x:l,m,r)
                        else if cond2 x then (l,x:m,r)
                        else (l,m,x:r)
                    where (l,m,r) = partition2 cond1 cond2 xs

qSort2ndPos [] = []
qSort2ndPos ((a,b):xs) = (qSort2ndPos l)++((a,b):(qSort2ndPos r))   --used only by qSort1stPos and the 1st values of all the tuples are equal
                where (l,m,r) = partition2 (\(x,y) -> (y<b)) (\(x,y) -> (y==b)) xs

qSort1stPos [] = []
qSort1stPos ((a,b):xs) = (qSort1stPos l)++(qSort2ndPos ((a,b):m))++(qSort1stPos r)
                where (l,m,r) = partition2 (\(x,y) -> (x<a)) (\(x,y) -> (x==a)) xs

sortEdgeList edges = qSort1stPos edges


buildAdjLists _ [] _ = []
buildAdjLists _ lists [] = lists
buildAdjLists n ((i,li):rest) ((e,f):edges) = if e==f then buildAdjLists n ((i,li):rest) edges
                                            else if i==e then if f>0 && f<=n then buildAdjLists n ((i,(li++[f])):rest) edges
                                                                else buildAdjLists n ((i,li):rest) edges
                                            else if e>i then (i,li):(buildAdjLists n rest ((e,f):edges))
                                            else buildAdjLists n ((i,li):rest) edges


buildAdjListsLight _ [] _ = []
buildAdjListsLight _ lists [] = lists
buildAdjListsLight n ((i,li):rest) ((e,f):edges) = if e==f then buildAdjListsLight n ((i,li):rest) edges
                                                    else if i==e then if f>0 && f<=n then buildAdjListsLight n ((i,(li++[f])):rest) edges
                                                                        else buildAdjListsLight n ((i,li):rest) edges
                                                    else if e>i then (i,(sort li)):(buildAdjListsLight n rest ((e,f):edges))
                                                    else buildAdjListsLight n ((i,li):rest) edges


-----------------------------------------------------------------------------------------


buildGraphStandard n edges = if n<=0 then Map.fromList []
                    else Map.fromList (buildAdjLists n [(i,[]) | i <- [1..n]] (sortEdgeList (addReverses edges)))

buildGraphLight n edges = if n<=0 then Map.fromList []      -- for first-coordinate-sorted edge lists - NEEDS BOTH (a,b) and (b,a) to be part of the list
                    else Map.fromList (buildAdjListsLight n [(i,[]) | i <- [1..n]] edges)

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