
stackLines :: [String] -> [String]
stackLines lines = takeWhile (\l -> not (isNumberLine l)) lines
    where
        isNumberLine :: String -> Bool
        isNumberLine s = '1' `elem` s 

moveLines :: [String] -> [String]
moveLines lines = tail (dropWhile (\l -> not (length l == 0)) lines)

parseStacks :: [String] -> [[Char]]
parseStacks values = internalParseStacks values (initStacks (columnCount (head values)))

    where

        initStacks :: Int -> [[Char]]
        initStacks n 
            | n == 0        = []
            | otherwise     = [] : initStacks (n-1)

        columnCount ::  String -> Int
        columnCount s = (length s + 1) `div` 4

        internalParseStacks :: [String] -> [[Char]] -> [[Char]]
        internalParseStacks values stacks = case values of 
            []      -> stacks
            (x:xs)  -> internalParseStacks xs (addToStacks x stacks)

        addToStacks :: String -> [[Char]] -> [[Char]]
        addToStacks s stacks = map (\(idx, stack) -> addToStack stack (s!!(idx*4+1))) (zip [0..] stacks)

        addToStack :: [Char] -> Char -> [Char]
        addToStack stack c 
            | c /= ' '  = stack ++ [c] 
            | otherwise = stack

parseMoves :: [String] -> [(Int,Int,Int)]
parseMoves values = internalParseMoves values

    where 
            internalParseMoves :: [String] -> [(Int,Int,Int)]
            internalParseMoves values = case values of
                []      -> []
                (x:xs)  -> internalParseMove x: internalParseMoves xs

            internalParseMove :: String -> (Int,Int,Int)
            internalParseMove s = (read (wrds!!1) :: Int, read (wrds!!3) :: Int, read (wrds!!5) :: Int)
                where 
                    wrds = words s

playMoves :: [(Int,Int,Int)] -> [[Char]] -> [[Char]]
playMoves moves stacks = case moves of 
     []         -> stacks
     (x:xs)     -> playMoves xs (playMove x stacks)

playMove :: (Int, Int, Int) -> [[Char]] -> [[Char]]
playMove (count, from, to) stacks = map (\(idx, stack) -> applyMove idx stack count (from-1) (to-1) stacks) (zip [0..] stacks)

applyMove :: Int -> [Char] -> Int -> Int -> Int -> [[Char]] -> [Char]
applyMove idx stack count from to stacks 
    | idx /= to && idx /= from          = stack
    | idx == to                         = take count (stacks!!from) ++ stack
    | idx == from                       = drop count stack

topAfterMoves :: [[Char]] -> [(Int, Int, Int)] -> [Char]
topAfterMoves stacks moves = map head (playMoves moves stacks)

main = do 
    content <- readFile "input-part01.txt"
    let l = lines content
    let sl = stackLines l
    let ml = moveLines l
    let stacks = parseStacks sl
    let moves = parseMoves ml
    let result = topAfterMoves stacks moves
    putStrLn (show result)
