import qualified Data.Map.Strict as Map
import BuildGraphs (buildGraph)

test 1 = buildGraph 0 [(1,3),(8,1),(0,1)] -- should be empty


test 2 = buildGraph 5 [(i,j) | i <- [1..5], j <- [i..5]]    -- C5


test 3 = buildGraph 5 [(i,j) | i <- [1..7], j <- [i..7]]    -- still C5, edges containing vertices with id>5 should be removed by the function


test 4 = buildGraph 100 ([(2*i,2*j) | i <- [1..50], j <- [i..50]]++[(2*i-1,2*j-1) | i <- [1..50], j <- [i..50]])
-- should create K50,50, where one group contans all odd ids and the second group contains all even ids (all ids are <=100)


test 5 = buildGraph 10000 ([(2*i,2*j) | i <- [1..5000], j <- [i..5000]]++[(2*i-1,2*j-1) | i <- [1..5000], j <- [i..5000]])
-- should create 5000,5000, where one group contans all odd ids and the second group contains all even ids (all ids are <=10000) - tests creation of big graphs

test _ = error"Nieznany numer testu"