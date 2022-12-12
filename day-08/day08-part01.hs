import Data.Array

type Forest = (Int, Array Int Int)

parse :: [String] -> Forest
parse values = (lineCount, array (0, length indicesAndValues - 1) indicesAndValues)
    where 
        lineCount = length values
        indicesAndValues = zip [0..] (map (\c -> read [c] :: Int) (concat values))


isVisible :: Forest -> (Int, Int) -> Bool
isVisible forest (x, y) = isVisibleFromNorth || isVisibleFromSouth || isVisibleFromWest || isVisibleFromEast

    where

        isVisibleFromNorth :: Bool
        isVisibleFromNorth  
            | y == 0        = True
            | otherwise     = null [(v,w) | let v  = x, w <- [0..y-1], validCoords v w && value v w >= value x y]

        isVisibleFromSouth :: Bool
        isVisibleFromSouth 
            | y == forestSize - 1   = True 
            | otherwise             = null [(v,w) | let v  = x, w <- [y+1..forestSize-1], validCoords v w && value v w >= value x y]

        isVisibleFromWest :: Bool
        isVisibleFromWest
            | x == 0    = True 
            | otherwise = null [(v,w) | let w = y, v <- [0..x-1], validCoords v w && value v w >= value x y]

        isVisibleFromEast :: Bool
        isVisibleFromEast 
            | x == forestSize - 1   = True
            | otherwise             = null [(v,w) | let w = y, v <- [x+1..forestSize-1], validCoords v w && value v w >= value x y]

        validCoords :: Int -> Int -> Bool
        validCoords a b  = a >= 0 && b >= 0 && a < forestSize && b < forestSize 

        forestSize      = fst forest
        forestValues    = snd forest

        value :: Int -> Int -> Int
        value a b = forestValues ! (a + b * forestSize)

numberOfVisibleTrees :: Forest -> Int
numberOfVisibleTrees forest = length [(x,y) | x <- [0..fst forest - 1], y <- [0..fst forest - 1], isVisible forest (x,y)]

main = do 
    content <- readFile "input-part01.txt"
    let forest = parse (lines content) 
    putStrLn (show  (numberOfVisibleTrees forest))
