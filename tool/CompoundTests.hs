import GraphAnalysis
import qualified Data.Map.Strict as Map

graphFromTests = do
    fileGraph <- buildGraphFromFile "testData.txt"
    let gr = sum2Graphs False fileGraph (buildKn 8)
    return gr


testVertexDegrees = do
    gr <- graphFromTests
    let degrees = vertexDegrees gr
    return degrees

testDegreeStats = do
    gr <- graphFromTests
    let degrees = degreeStats gr
    return degrees


test _ = error"Unknown test number"