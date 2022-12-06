module Day6 where

import Data.List

-- breaks a list into subsequences of length n
chunk :: Int -> [a] -> [[a]]
chunk num vals = map (reverse . take num . reverse) (drop num $ inits vals)

-- finds the location of the first sequence of n unique characters
locateMarker :: (Eq a) => Int -> [a] -> Int
locateMarker num vals = num + length invalidChunks
    where invalidChunks = takeWhile (\i -> (length . nub) i /= num) chunks
          chunks = chunk num vals

 

solve :: IO ()
solve = do
    input <- readFile "input/Day6.txt"
    
    let firstAnswer  = locateMarker 4  input
        secondAnswer = locateMarker 14 input 
        

    putStrLn $ "First  Solution:\t" ++ show firstAnswer
    putStrLn $ "Second Solution:\t" ++ show secondAnswer