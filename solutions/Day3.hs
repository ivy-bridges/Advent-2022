module Day3 where

import Data.List
import Data.Maybe



-- cuts a rucksack in half to get the two compartments
halves :: String -> (String , String)
halves rucksack = splitAt ((length rucksack) `div` 2) rucksack

-- returns the item shared by both halves of a rucksack
sharedItem :: String -> Char
sharedItem rucksack = head $ intersect firstSack secondSack
    where firstSack  = fst $ halves rucksack
          secondSack = snd $ halves rucksack

-- returns the priority score of a given item
priority :: Char -> Int
priority item = 1 + (fromMaybe 0 $ elemIndex item priorities)
    where priorities = ['a'..'z'] ++ ['A'..'Z']

-- groups the elves into groups of 3
triplets :: [String] -> [[String]]
triplets [] = []
triplets xs = (take 3 xs) : (triplets (drop 3 xs))

-- returns the item shared by all three members of a triple
tripletShared :: [String] -> Char
tripletShared [a,b,c] = head $ intersect (intersect a b) c

solve :: IO ()
solve = do
    input <- readFile "input/Day3.txt"
    
    let rucksacks = lines input
    
        -- first puzzle is to find the total priority of each elf's shared item
        totalPriorities = sum $ map (priority . sharedItem) rucksacks
        
        -- second puzzle is to find the total priority of each triple's badge
        -- the badge of a triple is the item they all share
        tripletGroups = triplets rucksacks
        totalBadgePriorities = sum $ map (priority . tripletShared) tripletGroups
        
    putStrLn $ "First  Solution:\t" ++ show totalPriorities
    putStrLn $ "Second Solution:\t" ++ show totalBadgePriorities 