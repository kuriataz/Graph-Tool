import GraphAnalysis
import qualified Data.Map.Strict as Map

graphFromTests = do
    fileGraph <- buildGraphFromFile "testData.txt"
    let gr = sumGraphs False [fileGraph, (buildKn 8), (buildCn 10)]
    return gr


testVertexDegrees = toIO vertexDegrees graphFromTests

testDegreeStats = toIO degreeStats graphFromTests

testComponents = toIO findComponents graphFromTests

testComponentDiam = toIO componentDiameters graphFromTests

testDistanceStats = toIO distanceStats graphFromTests

testPrint = printGraph (buildKmn 5 8)

testIOPrint = printIOGraph graphFromTests

testWrite = writeGraphToFile "result.txt" (buildKmn 5 10)

testIOWrite = writeIOGraphToFile "resultIO.txt" graphFromTests