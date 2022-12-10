import Data.List
import Data.Maybe

data NodeInfo = DirNodeInfo String | FileNodeInfo String Int deriving (Show)

isDirNodeInfo :: NodeInfo -> Bool
isDirNodeInfo (DirNodeInfo _)  = True 
isDirNodeInfo (FileNodeInfo _ _)  = False

isFileNodeInfo :: NodeInfo -> Bool
isFileNodeInfo nodeInfo = not (isDirNodeInfo nodeInfo)

data DirDesc = DirDesc String [NodeInfo] deriving (Show)

data ConsoleItem = ChangeDir String | List | DirName String | FileName String Int deriving (Show)

parseItems :: [String] -> [ConsoleItem]
parseItems lines = map parseItem lines

parseItem :: String -> ConsoleItem
parseItem s
    | "$ cd " `isPrefixOf` s      = ChangeDir (last wds)
    | "$ ls"  `isPrefixOf` s      = List
    | "dir "  `isPrefixOf` s      = DirName (last wds)
    | otherwise                   = FileName (last wds) (read (head wds) ::Int) 
    where wds = words s 

loadTree :: [ConsoleItem] -> [DirDesc]
loadTree items = iLoadTree items [] Nothing []
    where
        iLoadTree :: [ConsoleItem] -> [DirDesc] -> Maybe DirDesc -> [String] -> [DirDesc]
        iLoadTree items dirDescs currentDirDesc stack =
            case items of 
                []                          -> dirDescs ++ maybeToList currentDirDesc
                ChangeDir ".." : xs         -> iLoadTree xs (dirDescs ++ maybeToList currentDirDesc) Nothing (tail stack)
                ChangeDir "/" : xs          -> iLoadTree xs (dirDescs ++ maybeToList currentDirDesc) (newDirDesc "" stack) ("" : stack)
                ChangeDir dirName : xs      -> iLoadTree xs (dirDescs ++ maybeToList currentDirDesc) (newDirDesc dirName stack) (dirName : stack) 
                List : xs                   -> iLoadTree xs dirDescs currentDirDesc stack
                DirName dirName : xs        -> iLoadTree xs dirDescs (addNodeInfo (DirNodeInfo (fullName dirName stack)) (fromJust currentDirDesc)) stack
                FileName fileName size : xs -> iLoadTree xs dirDescs (addNodeInfo (FileNodeInfo (fullName fileName stack) size) (fromJust currentDirDesc)) stack 
        newDirDesc :: String -> [String] -> Maybe DirDesc
        newDirDesc dirName stack = Just (DirDesc (fullName dirName stack) [])

addNodeInfo :: NodeInfo -> DirDesc -> Maybe DirDesc
addNodeInfo nodeInfo (DirDesc name nodeInfos) = Just (DirDesc name (nodeInfos ++ [nodeInfo]))

fullName :: String -> [String] -> String
fullName "" []          = "/"
fullName s [_]          = "/" ++ s
fullName s (x:xs)       = fullName x xs ++ "/" ++ s 

findDir :: String -> [DirDesc] -> Maybe DirDesc
findDir dirName = find (\(DirDesc ddName _) -> dirName == ddName)

sizeOfDir :: DirDesc -> [DirDesc] -> Int
sizeOfDir dirDesc dirDescs = sizeOfFiles dirDesc  + sizeOfChildDirs dirDesc dirDescs
    
sizeOfFiles :: DirDesc -> Int
sizeOfFiles (DirDesc _ nodeInfos) = sum (map (\(FileNodeInfo _ size) -> size) (filter isFileNodeInfo nodeInfos))

sizeOfChildDirs :: DirDesc -> [DirDesc] -> Int
sizeOfChildDirs (DirDesc _ nodeInfos) dirDescs = sum (map 
                                        (\(DirNodeInfo dirName) -> sizeOfDir (fromJust (findDir dirName dirDescs)) dirDescs) 
                                        (filter isDirNodeInfo nodeInfos))

sumOfSizeAtMost100000 :: [DirDesc] -> Int
sumOfSizeAtMost100000 dirDescs = sum (filter (<=100000) (map (\dd -> sizeOfDir dd dirDescs) dirDescs))

neededSpace :: [DirDesc] -> Int
neededSpace dirDescs = 30000000 - (70000000 - sizeOfDir (fromJust (findDir "/" dirDescs)) dirDescs)

totalSizeOfSmallestRemovableDir :: [DirDesc] -> Int
totalSizeOfSmallestRemovableDir dirDescs = minimum matchingDirSizes
    where 
        space = neededSpace dirDescs
        dirSizes = map (\dd -> sizeOfDir dd dirDescs) dirDescs        
        matchingDirSizes = filter (\s -> s >= space) dirSizes 

main = do 
    content <- readFile "input-part01.txt"
    let items = parseItems (lines content)    
    let tree = loadTree items
    putStrLn (show  (totalSizeOfSmallestRemovableDir tree))
