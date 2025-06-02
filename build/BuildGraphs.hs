-- na razie tak, żeby było prościej pracować nad resztą rzeczy; na koniec można poszukać szybszej wersji (ta zbyt wymagająca dla dużych grafów gęstych)

module BuildGraphs (buildGraphStandard,buildGraphLight) where

import qualified Data.Map.Strict as Map
import Data.List

-- graf -> do wyrzucenia do osobnego pliku na koniec
type Graph = Map.Map Int [Int]

-- helping functions; they'reused only by other functions which check whether the arguments are correct
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