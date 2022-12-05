parseLines :: [String] -> [(Shape, Outcome)]
parseLines lines = map parseExpectedRound lines

data Shape = Rock | Paper | Scissors deriving (Eq, Show)
data Outcome = Loss | Draw | Win deriving (Eq, Show)

parseExpectedRound :: String -> (Shape, Outcome)
parseExpectedRound s = (parseShape (s!!0), parseOutcome (s!!2))

winAgainst :: Shape -> Shape
winAgainst s = case s of
    Rock        -> Scissors
    Paper       -> Rock
    Scissors    -> Paper

loseAgainst :: Shape -> Shape
loseAgainst s = case s of
    Rock        -> Paper
    Paper       -> Scissors
    Scissors    -> Rock

playedRound :: (Shape, Outcome) -> (Shape, Shape)
playedRound expectedRound = case expectedRound of
    (s, Draw)   -> (s,s)
    (s, Loss)   -> (s, winAgainst s)
    (s, Win)    -> (s, loseAgainst s)

parseOutcome :: Char -> Outcome
parseOutcome c
    | c == 'X' = Loss
    | c == 'Y' = Draw
    | c == 'Z' = Win

parseShape :: Char -> Shape
parseShape c
    | c == 'A'    = Rock
    | c == 'B'    = Paper
    | c == 'C'    = Scissors

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
    content <- readFile "sample-input-part01.txt"
    let rounds = parseLines (lines content)
    let result = sum (map scoreRound . playedRound rounds)
    printl result
