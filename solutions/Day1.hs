module Day1 where

import Data.List

-- haskell solution to https://adventofcode.com/2022/day/1
-- by ivy bridges





-- splits a list of lines into elves
splitElves :: [String] -> [[String]] 
splitElves input = filter (/= [""]) groups 
    where groups = groupBy (\x y -> not (null x || null y)) input 
    -- first groups sequences of nonempty strings together
    -- then removes the gaps inbetween
    
    
-- gets total calories held by one elf
totalCalories :: [String] -> Int
totalCalories = sum . (map read)    
 
 
-- 
solve :: IO ()
solve = do
    contents <- readFile "input/Day1.txt"
    
    -- splits the input into lines, then elves, then finds the totals
    let calorieCounts = lines contents
        elves = splitElves calorieCounts
        totals = map totalCalories elves
        -- totals holds the total calories held by each elf
        
        
        -- first puzzle is to find the calories carried by the elf with the most calories
        first_answer = maximum totals
        
        -- second puzzle is to find the calories carried by the top three elves with the most calories
        second_answer = sum . take 3 . reverse $ sort totals
    
    putStrLn $ "First  Solution:\t" ++ show first_answer
    putStrLn $ "Second Solution:\t" ++ show second_answer