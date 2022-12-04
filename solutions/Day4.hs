module Day4 where

import Data.List
import Data.Maybe

splitAtComma :: String -> (String, String)
splitAtComma str = (firstAssignment, secondAssignment)
    where firstAssignment = fst $ break (==',') str
          secondAssignment = tail $ snd $ break (==',') str
          
toRange :: String -> (Int, Int)
toRange assignment = (start, end)
    where start = read $ fst $ break (=='-') assignment
          end   = read $ tail $ snd $ break (=='-') assignment
          
testContains :: (String, String) -> Bool
testContains (firstAssignment, secondAssignment)
    | first < second = (firstEnd >= secondEnd)
    | second < first = (secondEnd >= firstEnd)
    | otherwise = True
    where (first, firstEnd) = toRange firstAssignment
          (second, secondEnd) = toRange secondAssignment
          
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
        parsedPairs = map (splitAtComma) pairs
        
        containedPairs = filter (testContains) parsedPairs
        overlapPairs   = filter (testOverlap)  parsedPairs
    
    
    putStrLn $ "First  Solution:\t" ++ show (length containedPairs)
    putStrLn $ "Second Solution:\t" ++ show (length overlapPairs)