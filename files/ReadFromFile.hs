module ReadFromFile () where

import qualified Data.Map.Strict as Map

-- graf -> do wyrzucenia do osobnego pliku na koniec
type Graph = Map.Map Int [Int]


dataFromFile pth = do
    fileData <- readFile ("./"++pth)
    let fileLines = lines fileData
    return fileLines


-- Zaczęte, na razie zostawione do czasu ustalenia co zrobićz tym, że trzeba trzymać IO