import Data.List

type Pos = (Int,Int)

data Command = Noop | Add Int deriving (Show)

parseCommands :: [String] -> [Command]
parseCommands = map parseCommand
    where
        parseCommand :: String -> Command
        parseCommand s
            | cmd == "noop" = Noop
            | cmd == "addx" = Add (read value :: Int)
            where 
                wrds = words s
                cmd = wrds!!0
                value = wrds!!1

sigVals :: [Command] -> [(Int,Int)]
sigVals commands = zip [1..] (internalApplyCommands commands 1)
    where 
        internalApplyCommands :: [Command] -> Int ->  [Int]
        internalApplyCommands [] v              = []
        internalApplyCommands (Noop:xs)     v   = v : internalApplyCommands xs v 
        internalApplyCommands (Add n:xs)    v   = v : (v : (internalApplyCommands xs (v+n)))

sigStrengths :: [(Int,Int)] -> [(Int,Int)]
sigStrengths vals = map computeStrength vals
    where
        computeStrength :: (Int, Int) -> (Int, Int)  
        computeStrength (index, value) = (index, index * value) 

strengthSum :: [(Int, Int)] -> Int
strengthSum vals = sum (map (\(idx, strg) -> strg) matchingStrength)
    where 
        matchingStrength = filter (\(idx, _) -> elem idx [20, 60, 100, 140, 180, 220]) vals

main = do 
    content <- readFile "sample-input-part01.txt"
    let commands = parseCommands (lines content)

    let vals = sigVals commands
    let strengths = sigStrengths vals
    putStrLn (show (strengthSum strengths))
