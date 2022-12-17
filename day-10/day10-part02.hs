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

sigVals :: [Command] -> [Int]
sigVals commands = internalApplyCommands commands 1
    where 
        internalApplyCommands :: [Command] -> Int ->  [Int]
        internalApplyCommands [] v              = []
        internalApplyCommands (Noop:xs)     v   = v : internalApplyCommands xs v 
        internalApplyCommands (Add n:xs)    v   = v : v : internalApplyCommands xs (v+n)

chunks :: [a] -> Int -> [[a]]
chunks l n = doChunk l n []
    where
        doChunk :: [a] -> Int -> [a] -> [[a]] 
        doChunk [] n currentChunk       = [currentChunk]
        doChunk (x:xs) n currentChunk
            | length currentChunk == n      = currentChunk : doChunk (x:xs) n []
            | otherwise                     = doChunk xs n (currentChunk ++ [x]) 

pixels :: [Int] -> [Char]
pixels values = internalComputePixels values 0
    where 
        internalComputePixels :: [Int] -> Int -> [Char]
        internalComputePixels [] _ = []
        internalComputePixels (x:xs) index
            | x-1 <= index && index <= x+1      = '#' : internalComputePixels xs (index+1)
            | otherwise                         = '.' : internalComputePixels xs (index+1)  

draw :: [[Char]] -> IO()
draw (x:xs) = do
    putStrLn x
    draw xs
draw [] = return ()

main = do 
    content <- readFile "input-part01.txt"
    let commands = parseCommands (lines content)

    let vals = sigVals commands
    let allChunks = chunks vals 40
    let pics = map pixels allChunks
    draw pics
