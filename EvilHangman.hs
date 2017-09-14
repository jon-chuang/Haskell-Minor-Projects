{-
This is a program that runs an evil hangman game.
This was my first haskell program.
The approach taken was not the most efficient (rather, using associative arrays,
which are implemented efficiently in haskell as Map in Data.Map, one can save a lot of trouble.).
I did not make use of many standard functions I didn't yet know about.
-}

import System.IO
import Control.Monad
import Data.List

main :: IO ()
main = do
    putStrLn ("This is a 'perfectly ordinary' game of hangman. (chuckle)")

    putStr("Enter dictionary file name (without .txt): ")
    hFlush stdout
    dictname <- getLine

    handle <- openFile (dictname ++ ".txt") ReadMode
    contents <- hGetContents handle
    let words = (lines contents)

    writeFile "Game_state.txt" ""

    let tableMissing = (tableMissingLengths 30 words [])
    putStrLn ("Checking for existence of words of length <= 30... \nNone found for: \n" ++ show tableMissing)

    wl <- getWordLength tableMissing

    let wordset = getWordsOfLength wl words
    let l = length wordset

    putStrLn $ "Loading database...\n" ++ "Possible words: " ++ (show l)

    putStr ("Enter maximum no. of tries: ")
    hFlush stdout
    noTries <- getLine
    let nt = read noTries :: Int
    putStrLn ""

    play [] wordset wl [] (map (\x -> '-')(wordset !! 0)) nt
    temp <- getLine
    hClose handle
    return ()

tableMissingLengths :: Int -> [[Char]] -> [Int] -> [Int]
tableMissingLengths n words table
    |n /= 0 = do
        let bool = (elem n (map length words))
        let tableTemp = addToList bool table n
        let m = n - 1
        tableMissingLengths m words tableTemp
    |otherwise = table

addToList :: Bool -> [Int] -> Int -> [Int]
addToList bool table n
    |not bool = (++[n]) table
    |otherwise = table

getWordLength :: [Int] -> IO Int
getWordLength tableMissing = do
    putStr ("Enter word length: ")
    hFlush stdout
    wordlength <- getLine
    let wl = read wordlength :: Int
    if ((wl `elem` tableMissing) || wl > 30)
        then do putStrLn ("Invalid word length")
                getWordLength tableMissing
        else do return wl

getWordsOfLength n (wordshead:wordstail)
    |length(wordshead) == n && 
     foldl (&&) True (map (flip elem ("abcdefghijklmnopqrstuvwxyz" :: String) ) wordshead) 
                            =  (wordshead : (getWordsOfLength n wordstail) )
    |otherwise              = getWordsOfLength n wordstail
getWordsOfLength n [] = []

toList :: Char -> [Char]
toList a = [a]

checkGuessed guessed = do
    let alphabet = "abcdefghijklmnopqrstuvwxyz" :: String
    let truthList = map (flip elem guessed) alphabet
    putStr "Used letters: "
    sequence (zipWith (printIfTrue) truthList alphabet)
    putStrLn ""

printIfTrue x y
    |x = putStr (toList y ++ " ")
    |otherwise = putStr ""

play :: [Char] -> [[Char]] -> Int -> [(Char, [Int])] -> [Char] -> Int -> IO ()
play guessed wordset wl listConditions known wc = do
    if ('-' `elem` known) && (wc /= 0)
        then do let howManyGuesses = 
                        if wc == 1 then ("1 guess" :: String) 
                        else if wc == 0 then ("no guesses":: String) 
                        else show wc ++ (" guesses":: String)
                putStrLn $ "You have " ++ howManyGuesses ++ " left."
                checkGuessed guessed
                appendFile "Game_state.txt" $ "Word: " ++ known ++"\n"
                putStrLn $ "Word: " ++ known
                putStr $ "Enter a guess: "
                hFlush stdout
                line <- getLine
                let char = (head line)
                newConditions <- selectConditionGivenLetter char wordset wl listConditions
                appendFile "Game_state.txt" $ "Current Conditions:\n" ++ show newConditions ++ "\n"
                --putStrLn $ "Current Conditions:\n" ++ show newConditions ++ "\n" --dev debug
                let (newknown, newguessed, newwc) = getVariablesForPassing newConditions char known guessed wc
                play newguessed wordset wl newConditions newknown newwc
    else if (wc == 0)
        then do putStrLn $ "You've ran out of guesses. The evil hangman has won today.\nThe word was: " ++ 
                           Maybe.fromJust ( find  ((all (==True)) . (mapf (map checkCondTrue listConditions))) wordset )
        else do putStrLn $ "You've won! The word was: " ++ known

getVariablesForPassing :: [(Char, [Int])] -> Char -> [Char] -> [Char] -> Int -> ([Char], [Char], Int)
getVariablesForPassing newConditions char known guessed wc
    | (snd (head newConditions) /= []) && char `elem` ("abcdefghijklmnopqrstuvwxyz" :: String) = (knowntemp, guessed, wc)
    | char `elem` ("abcdefghijklmnopqrstuvwxyz" :: String)  && char `notElem` guessed          = (known, char:guessed, wc - 1)
    | otherwise                                                                                = (known, guessed, wc)
    where knowntemp = foldlf (mapf (map replaceItemInPosition (snd (head newConditions) ) ) char) known

selectConditionGivenLetter :: Char -> [[Char]] -> Int -> [(Char, [Int])] -> IO [(Char, [Int])]
selectConditionGivenLetter char wordset wl listConditions = do
    let maxNo = if wl >= 10
                   then 3
                   else 2
    let countList = [(countSatisfy ((char, letterPositionList):listConditions) wordset , (char, letterPositionList) ) | 
                     letterPositionList <- foldl (++) [] (map (increasingSequences 0 (wl-1)) [0..maxNo] )  ]
    -- We improve performance by ignoring cases having more than 2 or 3 occurences of letter in word.
    appendFile "Game_state.txt" $ "Partition size given additional condition:\n" ++ 
                                   (foldl (++) "" (map ((++"\n").show)countList ) ) ++ "\n"
    let countAndSelectedList = getMaxCount countList
    appendFile "Game_state.txt" $ "Count associated with selected list:\n" ++ show countAndSelectedList  ++ "\n"
    let selectedList = snd countAndSelectedList
    let number = length $ snd selectedList
    let areNumber = if number == 1 then "is 1 " ++ show char ++ "\n"
                                   else "are " ++ show number ++ show char ++ "s\n"
    if (\(x, y) -> y) selectedList == []
        then putStrLn $ "Sorry, there are no " ++ show char ++ "s\n"
        else putStrLn $ "Yes, there " ++ areNumber
    return (selectedList: listConditions)

getMaxCount :: [(Int, (Char, [Int]))] -> (Int, (Char, [Int]))
getMaxCount ((x1, y1):(x2, y2):xs)
    |x1 >= x2 = getMaxCount ((x1, y1):xs) --Get the empty list unless there is bigger partition
    |otherwise = getMaxCount ((x2, y2):xs)
getMaxCount ((x, y):[]) = (x, y)
getMaxCount [] = error "No conditions set"

countSatisfy :: [(Char, [Int])] -> [[Char]] -> Int
countSatisfy listConditions wordset =
    foldl (+) 0 $ map (boolToBin . (foldl (&&) True)) (map (mapf (map checkCondTrue listConditions)) wordset)
    --                                                            {map Bool function to a list of conditions}
    --                                                             -> {list of curried Bool functions}
    --                                                {map the mapf version of {list of curried Bool functions} to the wordset}

boolToBin :: Bool -> Int
boolToBin bool
    |bool      = 1
    |otherwise = 0

checkCondTrue :: (Char, [Int]) -> [Char] -> Bool
checkCondTrue (letterChar, letterPositionList) word
    = (foldl (&&) True $ map (letterChar ==) (map (word !!) letterPositionList)) && 
                        (letterChar `notElem` (removeItemsInListOfPositions (quicksort letterPositionList) word) )
    -- {Check that letterChar is in all the positions on our list}              AND               {Nowhere else!}

removeItemsInListOfPositions :: [Int] -> [a] -> [a]
removeItemsInListOfPositions (x:xs) word =
     removeItemsInListOfPositions xs (removeItemsInPosition x word)
removeItemsInListOfPositions []     word = word

removeItemsInPosition :: Int -> [a] -> [a]
removeItemsInPosition pos x = (take pos x) ++ (drop (pos + 1) x)

replaceItemInPosition :: Int -> a -> [a] -> [a]
replaceItemInPosition pos x y = (take pos y) ++ [x] ++ (drop (pos + 1) y)

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort greater) ++ [p] ++ (quicksort lesser)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

increasingSequences :: Int -> Int -> Int -> [[Int]]
increasingSequences min max 0 = [[]] --terminate list once count = 0
increasingSequences min max count | max-min+1 < count = [] 
--return the empty list if there aren't enough values between max and min inclusive to form a list of length "count"
increasingSequences min max count = do
    h <- [min .. max] --basically a for loop, for all h in list.
    t <- increasingSequences (h+1) max (count-1) --nested for loop, over a range of values determined by h
    return (h:t)

-- {Maps a list of functions to a single argument to obtain a list of results}
mapf :: [(a -> b)] -> a -> [b]
mapf (f:fs) x = f x : mapf fs x
mapf [] _ = []

-- Applies foldl to a list of functions given initial argument. Can be seen as composing all functions in list.
-- Another implementation: (foldl (.) id listf) arg
foldlf :: [(a->a)] -> a -> a
foldlf (f:fs) x = foldlf fs (f x)
foldlf [] x     = x
