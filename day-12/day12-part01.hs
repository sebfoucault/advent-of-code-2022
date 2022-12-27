import Data.Array
import Data.List
import Data.Maybe
import Data.Char

data Graph = Graph (Array Int Char) Int deriving (Show)

parseGraph :: [String] -> (Graph, (Int, Int))
parseGraph lines = (Graph valuesArray colCount, (indexOfStart, indexOfEnd))
    where
        rawValues = concat lines
        colCount = length (lines!!0)
        values = replace [('S','a'),('E','z')] rawValues
        valuesArray = listArray (0, length values - 1) values

        indexOfStart = fromJust  (elemIndex 'S' rawValues)
        indexOfEnd = fromJust (elemIndex 'E' rawValues)
        replace :: [(Char, Char)] -> String -> String
        replace replacements s = map (\c -> fromMaybe c (lookup c replacements)) s

hasEdge :: Graph -> Int -> Int -> Bool
hasEdge g@(Graph values colCount) from to 
    | ord (values!to) - ord (values!from) > 1   = False
    | not (isAdjacent g from to)                = False
    | otherwise                                 = True

isAdjacent :: Graph -> Int -> Int -> Bool
isAdjacent g@(Graph _ colCount) a b
    | a > b         = isAdjacent g b a  
    | a == b        = False
    | otherwise     = b == a+1 || b == a+colCount

shortestPath :: Graph -> Int -> Int -> Array Int Int
shortestPath g@(Graph values colCount) start end = innerShortestPaths initVisited initPredecessors initShortestDistances
    where 
        innerShortestPaths :: Array Int Bool -> Array Int Int -> Array Int Int -> Array Int Int
        innerShortestPaths visited predecessors shorttestDistances 
            | allVisited        = shorttestDistances
            | otherwise         = 
                let nu = neareastUnvisited 
                innerShortestPaths = elems shorttestDistances

            where
                
                allVisited :: Bool
                allVisited = length visited == length values

                neareastUnvisited :: Int
                neareastUnvisited = indexOfNearest (filter (\(i, e) -> visited!i == False) (assocs shorttestDistances))

                indexOfNearest :: [(Int, Int)] -> Int
                indexOfNearest l = foldl () 

        initVisited :: (Array Int Bool)
        initVisited = arrayFromList [False | x <- [0..length values - 1]]

        initPredecessors :: (Array Int Int)
        initPredecessors = arrayFromList [(-1) | x <- [0..length values - 1]]

        initShortestDistances :: (Array Int Int)
        initShortestDistances = arrayFromList [999999 | x <- [0..length values - 1]]

arrayFromList :: [a] -> Array Int a
arrayFromList l = array (0, length l - 1) (zip [0..] l)

main = do 
    content <- readFile "sample-input-part01.txt"
    let g = parseGraph (lines content)
    putStrLn (show g)
