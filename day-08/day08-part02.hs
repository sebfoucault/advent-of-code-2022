import Data.Array

type Forest = (Int, Array Int Int)

parse :: [String] -> Forest
parse values = (lineCount, array (0, length indicesAndValues - 1) indicesAndValues)
    where 
        lineCount = length values
        indicesAndValues = zip [0..] (map (\c -> read [c] :: Int) (concat values))

scenicScore :: Forest -> (Int, Int) -> Int
scenicScore forest (x, y) =
    northScenicScore x (y-1) * southScenicScore x (y+1) * westScenicScore (x-1) y * eastScenicScore (x+1) y 
    where

        northScenicScore a b
            | not (validCoords a b)     = 0 
            | b == 0                    = 1
            | value a b >= value x y    = 1 
            | otherwise                 = 1 + northScenicScore a (b-1) 

        southScenicScore a b
            | not (validCoords a b)     = 0 
            | b == forestSize-1         = 1
            | value a b >= value x y    = 1 
            | otherwise                 = 1 + southScenicScore a (b+1) 

        westScenicScore a b
            | not (validCoords a b)     = 0 
            | a == 0                    = 1
            | value a b >= value x y    = 1 
            | otherwise                 = 1 + westScenicScore (a-1) b 

        eastScenicScore a b
            | not (validCoords a b)     = 0 
            | a == forestSize-1         = 1
            | value a b >= value x y    = 1 
            | otherwise                 = 1 + eastScenicScore (a+1) b

        validCoords :: Int -> Int -> Bool
        validCoords a b  = a >= 0 && b >= 0 && a < forestSize && b < forestSize 

        value :: Int -> Int -> Int
        value a b = forestValues ! (a + b * forestSize)

        forestSize      = fst forest
        forestValues    = snd forest

largestScenicScore :: Forest -> Int
largestScenicScore forest = maximum [scenicScore forest (x,y) | x <- [0..fst forest - 1], y <- [0..fst forest - 1]]

main = do 
    content <- readFile "input-part01.txt"
    let forest = parse (lines content) 
    putStrLn (show  (largestScenicScore forest))
