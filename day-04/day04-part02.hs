
parseAssignments :: [String] -> [((Int, Int), (Int,Int))]
parseAssignments values = map parseAssignment values

parseAssignment :: String -> ((Int, Int), (Int,Int))
parseAssignment s = 
    mapTuple parseSection (strSplit ',' s)
    where 
        parseSection :: String -> (Int, Int) 
        parseSection section = mapTuple (\range -> read range :: Int) (strSplit '-' section)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

strSplit :: Char -> String -> (String, String)
strSplit separator value = 
    (fst spt, tail (snd spt))
    where 
        spt = break (\c -> c == separator) value

overlap :: ((Int, Int), (Int,Int)) -> Bool
overlap (assig1, assig2) =
    hasBoundaryIn assig1 assig2 == True || hasBoundaryIn assig2 assig1 == True
    where 
        hasBoundaryIn :: (Int, Int) -> (Int, Int) -> Bool
        hasBoundaryIn assigToTest refAssig 
            | fst assigToTest >= fst refAssig && fst assigToTest <= snd refAssig = True
            | snd assigToTest >= fst refAssig && snd assigToTest <= snd refAssig = True
            | otherwise = False

main = do 
    content <- readFile "input-part01.txt"
    let assigs = parseAssignments (lines content)
    let result = length (filter overlap assigs)
    putStrLn (show result)
