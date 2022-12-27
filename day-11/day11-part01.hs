import Data.Array
import Data.List

data Operator = Add | Sub | Mul | Div
data Operand = ConstantOperand Int | OldValueOperand
data Expression = Expression Operand Operator Operand

type Divisor = Int
type IfDivideIsTrue = Int
type IfDivideIsFalse = Int

data WorryTest = WorryTest Divisor IfDivideIsTrue IfDivideIsFalse
newtype Monkey = Monkey [Int] 

type MonkeyItems = Array Int [Int]

data ItemMove = ItemMove Int Int Int 

parseMonkey :: [String] -> Monkey
parseMonkey (_:startingItems:expression:divisor:ifTrue:ifFalse:xs) =
    Monkey (parseStartingItems startingItems)
    where
        parseStartingItems s = 

giveItem :: ItemMove -> MonkeyItems -> MonkeyItems
giveItem (ItemMove monkeyFrom monkeyTo itemNumber) initItems =
    initItems //  [(monkeyFrom, itemRemoved), (monkeyTo, itemAdded)]
    where
        itemRemoved = delete itemNumber (initItems ! monkeyFrom)
        itemAdded = itemNumber :  (initItems ! monkeyTo)

parseMonkey :: [String] -> Maybe Monkey
parseMonkey lines = Nothing

chunks :: [String] -> [[String]]
chunks = split (== "")

split :: (a -> Bool) -> [a] ->  [[a]]
split splitPredicate l = doSplit splitPredicate l []
    where
        doSplit :: (a -> Bool) -> [a] -> [a] -> [[a]] 
        doSplit p [] currentChunk = [currentChunk]
        doSplit p (x:xs) currentChunk
            | p x           = currentChunk : doSplit p xs []
            | otherwise     = doSplit p xs (currentChunk ++ [x]) 

main = do 
    content <- readFile "sample-input-part01.txt"
    let commands = (lines content)
    putStrLn (show (chunks commands))
