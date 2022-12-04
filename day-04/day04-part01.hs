
parseAssignments :: [String] -> [((Int, Int), (Int,Int))]
parseAssignments values = map parseAssignment values

parseAssignment :: String -> ((Int, Int), (Int,Int))
parseAssignment s = 
    mapTuple parseSection (strSplit ',' s)
    where 
        parseSection :: String -> (Int, Int) 
        parseSection section = mapTuple (\range -> read range :: Int) (strSplit '-' section)

strSplit :: Char -> String -> (String, String)
strSplit separator value = 
    (fst spt, tail (snd spt))
    where 
        spt = break (\c -> c == separator) value

fullContainment :: ((Int, Int), (Int,Int)) -> Bool
fullContainment t
    | a <= x && b >= y  = True
    | x <= a && y >= b  = True
    | otherwise         = False
    where 
        a = fst (fst t)
        b = snd (fst t)
        x = fst (snd t)
        y = snd (snd t)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

main = do 
    content <- readFile "input-part01.txt"
    let assigs = parseAssignments (lines content)
    let result = length (filter fullContainment assigs)
    putStrLn (show result)
