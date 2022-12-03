module Day3 where

import Data.List
import Data.Maybe

-- cuts a rucksack in half to get the two compartments
halves :: String -> (String , String)
halves rucksack = splitAt ((length rucksack) `div` 2) rucksack

sharedItem :: String -> Char
sharedItem sack = head $ intersect firstSack secondSack
    where firstSack  = fst $ halves sack
          secondSack = snd $ halves sack

priority :: Char -> Int
priority chr = 1 + (fromMaybe 0 $ elemIndex chr priorities)
    where priorities = ['a'..'z'] ++ ['A'..'Z']

triples :: [String] -> [[String]]
triples [] = []
triples xs = (take 3 xs) : (triples (drop 3 xs))

tripleShared :: [String] -> Char
tripleShared [a,b,c] = head $ intersect (intersect a b) c

solve :: IO ()
solve = do
    input <- readFile "input/Day3.txt"
    
    let rucksacks = lines input
        totalPriorities = sum $ map (priority . sharedItem) rucksacks
        
        tripleGroups = triples rucksacks
        totalBadgePriorities = sum $ map (priority . tripleShared) tripleGroups
        
    putStrLn $ "First  Answer : " ++ show totalPriorities
    putStrLn $ "Second Answer : " ++ show totalBadgePriorities 