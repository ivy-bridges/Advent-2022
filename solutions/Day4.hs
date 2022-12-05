module Day4 where

import Data.List
import Data.Maybe



-- splits a string at a character, and removes the character from the second item
splitAtChar :: Char -> String -> (String, String)
splitAtChar chr str = (firstItem, tail secondItem)
    where (firstItem, secondItem) = break (==chr) str
          
-- takes an elf's section assignment and returns the starting and ending sections  
-- sections are split at a - character        
toRange :: String -> (Int, Int)
toRange assignment = (read start, read end)
    where (start, end) = splitAtChar '-' assignment

-- given a pair of ranges, test whether one contains the other          
testContains :: (String, String) -> Bool
testContains (firstAssignment, secondAssignment)
    | first < second = (firstEnd >= secondEnd)
    | second < first = (secondEnd >= firstEnd)
    | otherwise = True
    where (first, firstEnd) = toRange firstAssignment
          (second, secondEnd) = toRange secondAssignment

-- given a pair of ranges, test whether one overlaps with the other         
testOverlap :: (String, String) -> Bool
testOverlap (firstAssignment, secondAssignment)
    | first < second = (firstEnd >= second)
    | second < first = (secondEnd >= first)
    | otherwise = True
    where (first, firstEnd) = toRange firstAssignment
          (second, secondEnd) = toRange secondAssignment



solve :: IO ()
solve = do
    input <- readFile "input/day4.txt"
    
    let pairs = lines input
        -- pairs are split at a , character
        parsedPairs = map (splitAtChar ',') pairs
        
        -- first puzzle is to count the number of pairs where one contains the other
        containedPairs = filter (testContains) parsedPairs
        
        -- second puzzle is to count the number of pairs where one overlaps with the other
        overlapPairs   = filter (testOverlap)  parsedPairs
    
    
    putStrLn $ "First  Solution:\t" ++ show (length containedPairs)
    putStrLn $ "Second Solution:\t" ++ show (length overlapPairs)
