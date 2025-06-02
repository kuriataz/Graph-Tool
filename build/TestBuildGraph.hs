import qualified Data.Map.Strict as Map
import BuildGraphs (buildGraphStandard,buildGraphLight)

test 1 = buildGraphStandard 0 [(1,3),(8,1),(0,1)] -- should be empty


test 2 = buildGraphStandard 5 [(i,j) | i <- [1..5], j <- [i..5]]    -- C5


test 3 = buildGraphStandard 5 [(i,j) | i <- [1..7], j <- [i..7]]    -- still C5, edges containing vertices with id>5 should be removed by the function


test 4 = buildGraphStandard 100 ([(2*i,2*j) | i <- [1..50], j <- [i..50]]++[(2*i-1,2*j-1) | i <- [1..50], j <- [i..50]])
-- should create K50,50, where one group contans all odd ids and the second group contains all even ids (all ids are <=100)


test 5 = buildGraphStandard 10000 ([(2*i,2*j) | i <- [1..5000], j <- [i..5000]]++[(2*i-1,2*j-1) | i <- [1..5000], j <- [i..5000]])
-- should create 5000,5000, where one group contans all odd ids and the second group contains all even ids (all ids are <=10000) - tests creation of big graphs

test _ = error"Nieznany numer testu"



test2 1 = buildGraphLight 0 [(1,3),(8,1),(0,1)] -- should be empty


test2 2 = buildGraphLight 5 [(i,j) | i <- [1..5], j <- [1..5]]    -- C5


test2 3 = buildGraphLight 5 [(i,j) | i <- [1..7], j <- [1..7]]    -- still C5, edges containing vertices with id>5 should be removed by the function


-- te dwa testy poniżej do dokończenia!!!

test2 4 = buildGraphLight 100 [(2*i+k,2*j+k) | i <- [1..50], j <- [1..50], k<- [0,1]]
-- should create K50,50, where one group contans all odd ids and the second group contains all even ids (all ids are <=100)


test2 5 = buildGraphLight 10000 [(2*i+k,2*j+k) | i <- [1..5000], j <- [1..5000], k<- [0,1]]
-- should create 5000,5000, where one group contans all odd ids and the second group contains all even ids (all ids are <=10000) - tests creation of big graphs

test2 _ = error"Nieznany numer testu"