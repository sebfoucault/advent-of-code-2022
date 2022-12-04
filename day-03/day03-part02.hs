import Data.Char
import Data.List

parseGroups :: [String] -> [(String, String, String)]
parseGroups values =
    case values of 
        []                                  -> []
        (first:(second:(third:rest)))       -> [(first, second, third)] ++ parseGroups rest

itemTypePriority :: Char -> Int
itemTypePriority c
    | isLower c = ord c - ord 'a' + 1
    | isUpper c = ord c - ord 'A' + 27

badgeOfGroup :: (String, String, String) -> Char
badgeOfGroup (first, second, third) =
    head [i | i <- first, i `elem` second && i `elem` third]

main = do 
    content <- readFile "input-part01.txt"
    let groups = parseGroups(lines content)
    let result = sum (map itemTypePriority (map badgeOfGroup groups))
    putStrLn (show result)
    