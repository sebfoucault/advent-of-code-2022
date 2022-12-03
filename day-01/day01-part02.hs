import Data.List

parseLines :: [String] -> [[Int]]
parseLines lines = internalParseLines lines []
    where 
        internalParseLines :: [String] -> [[Int]] -> [[Int]]
        internalParseLines lines parsedLines =
            case (lines, parsedLines) of
                ([], _)             -> parsedLines
                (("":t), _)         -> internalParseLines t (parsedLines ++ [[]])
                ((h:t), [])         -> internalParseLines t (parsedLines ++ [[read h :: Int]])
                ((h:t), _)          -> internalParseLines t ((init parsedLines) ++ [(last parsedLines) ++ [read h :: Int]])

highestCalories :: [[Int]] -> Int
highestCalories values = maximum (map (\vals -> sum vals) values)

sumOfTopHighestCalories :: [[Int]] -> Int -> Int
sumOfTopHighestCalories values n = sum (take n (reverse (sort (map (\vals -> sum vals) values))))

main = do 
    content <- readFile "input-part01.txt"
    let result = sumOfTopHighestCalories (parseLines (lines content)) 3
    putStrLn (show result)