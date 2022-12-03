parseLines :: [String] -> [(Shape, Shape)]
parseLines lines = map (\l -> parseRound l) lines

data Shape = Rock | Paper | Scissors deriving Eq

parseRound :: String -> (Shape, Shape)
parseRound s = (parseShape (s!!0), parseShape (s!!2))

parseShape :: Char -> Shape
parseShape c
    | c == 'X' || c == 'A'    = Rock
    | c == 'Y' || c == 'B'    = Paper
    | c == 'Z' || c == 'C'    = Scissors

scoreRound :: (Shape, Shape) -> Int
scoreRound x = scoreShape x + scoreOutcome x

scoreShape :: (Shape, Shape) -> Int
scoreShape x = case x of
    (_, Rock) -> 1
    (_, Paper) -> 2
    (_, Scissors) -> 3

scoreOutcome :: (Shape, Shape) -> Int
scoreOutcome x
    | fst x == snd x                = 3
    | x == (Rock,       Paper)      = 6
    | x == (Paper,      Scissors)   = 6
    | x == (Scissors,   Rock)       = 6
    | otherwise                     = 0    

main = do 
    content <- readFile "input-part01.txt"
    let rounds = parseLines (lines content)
    let result = sum (map (\r -> scoreRound r) rounds)
    putStrLn (show result)
