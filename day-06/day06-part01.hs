import Data.List

firstMarkerAfter :: String -> Int
firstMarkerAfter s = internalFirstMarkerAfter s 0
  where 
    internalFirstMarkerAfter :: String -> Int -> Int
    internalFirstMarkerAfter s n
      | n+4 > length s                  = (-1)
      | length (nub (take 4 s)) == 4    = n + 4
      | otherwise                       = internalFirstMarkerAfter (tail s) n+1

main = do 
    content <- readFile "input-part01.txt"
    putStrLn (show (firstMarkerAfter content))

