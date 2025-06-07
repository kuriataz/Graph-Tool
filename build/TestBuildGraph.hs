import qualified Data.Map.Strict as Map
import BuildGraphs (buildGraph,insertVert,insertNVerts,insertEdges,buildKn,buildKmn,buildCn,sum2Graphs,sumGraphs)

test 1 = buildGraph 0 [(1,3),(8,1),(0,1)] -- should be empty


test 2 = buildGraph 5 [(i,j) | i <- [1..5], j <- [1..5]]    -- K5


test 3 = buildGraph 5 [(i,j) | i <- [1..7], j <- [1..7]]    -- still K5, edges containing vertices with id>5 should be removed by the function


test 4 = buildGraph 100 [(i,j) | i <- [1..100], j <- [1..100], (mod i 2)==(mod j 2)]
-- should create K50,50, where one group contans all odd ids and the second group contains all even ids (all ids are <=100)

test 5 = buildGraph 10000 [(i,j) | i <- [1..10000], j <- [1..10000], (mod i 2)==(mod j 2)]
-- should create K5000,5000, where one group contans all odd ids and the second group contains all even ids (all ids are <=10000) - tests creation of big graphs

test _ = error"Unknown test number"




testKn = take 3 (Map.toList(buildKn 100000))

testKmn = take 3 (Map.toList(buildKmn 2 100000))

testCn = take 10 (Map.toList(buildCn 1000000))

testSum2 isStrict = sum2Graphs isStrict (buildCn 10) (buildKn 5)

testSum isStrict = sumGraphs isStrict [(buildCn n) | n <- [1..10]]


testVert = insertVert (buildKn 5) -- should be K5 + isolated vertex with id 6

testNVerts = insertNVerts (buildKn 5) 10 -- should be K5 + isolated vertices with ids 6..15

testEdgess = insertEdges (insertNVerts (buildKn 5) 10) [(i,2) | i <- [6..15]] -- should be K5 + vertices with ids 6..15 connected with vertex 2