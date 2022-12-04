import Data.Char
import Data.List

itemTypesInTwoCompartments :: String -> [Char]
itemTypesInTwoCompartments s = 
        nub [i | i <- c1, i `elem` c2]
    where
        halfLength = length s `div` 2 
        c1 = take halfLength s
        c2 = drop halfLength s

prioritiesOfItemTypesInTwoCompartments :: String -> [Int]
prioritiesOfItemTypesInTwoCompartments s = map itemTypePriority (itemTypesInTwoCompartments s)

itemTypePriority :: Char -> Int
itemTypePriority c
    | isLower c = ord c - ord 'a' + 1
    | isUpper c = ord c - ord 'A' + 27

main = do 
    content <- readFile "input-part01.txt"
    let result = sum (concat (map prioritiesOfItemTypesInTwoCompartments (lines content)))
    putStrLn (show result)
