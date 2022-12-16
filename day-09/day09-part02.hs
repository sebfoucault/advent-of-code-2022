import Data.List

type Pos = (Int,Int)

data Direction = Up | Down | Left | Right deriving (Eq)
type Move = (Direction, Int)

parseMoves :: [String] -> [Move]
parseMoves = map parseMove
    where
        parseMove :: String -> Move
        parseMove s  
            | direction == "U" = (Main.Up, read distance :: Int)
            | direction == "D" = (Main.Down, read distance :: Int)
            | direction == "L" = (Main.Left, read distance :: Int)
            | direction == "R" = (Main.Right, read distance :: Int)
            where 
                wrds = words s
                direction = wrds!!0
                distance = wrds!!1

splitMoves :: [Move] -> [Move]
splitMoves = concatMap splitMove
    where
        splitMove :: Move -> [Move]
        splitMove (direction, 0) = []
        splitMove (direction, n) = (direction, 1) : splitMove (direction, n-1)

move :: Pos -> Move -> Pos
move (x,y) move = case move of
    (Main.Up, n)     -> (x, y+n)
    (Main.Down, n)   -> (x, y-n)
    (Main.Left, n)   -> (x-n, y)
    (Main.Right, n)   -> (x+n, y)

applyMoves :: [Move] -> Pos -> [Pos]
applyMoves [] pos = [pos]
applyMoves (x:xs) pos = pos : applyMoves xs nextPos
    where nextPos = move pos x

calculateNextTailPos :: Pos -> Pos -> Pos
calculateNextTailPos (x, y) tailPos 
    | tailPos == (x,y)           = (x,y)
    | touch (x,y) tailPos        = tailPos
    | tailPos == (x-2, y)        = (x-1, y)
    | tailPos == (x+2, y)        = (x+1, y)
    | tailPos == (x, y-2)        = (x, y-1)
    | tailPos == (x, y+2)        = (x, y+1)
    | otherwise                  = (fst tailPos + unit diffX, snd tailPos + unit diffY)
    where 
        diffX = x - fst tailPos
        diffY = y - snd tailPos
        unit n 
            | n < 0         = -1
            | n > 0         = 1
            | otherwise     = 0

touch :: Pos -> Pos -> Bool
touch (x,y) tailPos
    | tailPos == (x+1, y) = True
    | tailPos == (x-1, y) = True
    | tailPos == (x, y+1) = True
    | tailPos == (x, y-1) = True
    | tailPos == (x+1, y+1) = True
    | tailPos == (x+1, y-1) = True
    | tailPos == (x-1, y+1) = True
    | tailPos == (x-1, y-1) = True 
    | otherwise             = False   

tailPositions :: Pos -> [Pos] -> [Pos]
tailPositions _ []                              = []
tailPositions initTailPosition headPositions    = nextTailPosition : tailPositions nextTailPosition (tail headPositions)
    where 
        nextTailPosition = calculateNextTailPos (head headPositions) initTailPosition

main = do 
    content <- readFile "input-part01.txt"
    let moves = parseMoves (lines content)
    let splits = splitMoves moves
    let allHeadPos = applyMoves splits (0,0)
    let allTailPos0 = tailPositions (0,0) allHeadPos
    let allTailPos1 = tailPositions (0,0) allTailPos0
    let allTailPos2 = tailPositions (0,0) allTailPos1
    let allTailPos3 = tailPositions (0,0) allTailPos2
    let allTailPos4 = tailPositions (0,0) allTailPos3
    let allTailPos5 = tailPositions (0,0) allTailPos4
    let allTailPos6 = tailPositions (0,0) allTailPos5
    let allTailPos7 = tailPositions (0,0) allTailPos6
    let allTailPos8 = tailPositions (0,0) allTailPos7
    
    putStrLn (show (length (nub allTailPos8)))
