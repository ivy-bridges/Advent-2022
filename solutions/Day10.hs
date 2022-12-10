module Day10 where

import Data.List
import Data.Ord



-- given a start (x, cycles) value and an instruction
-- returns the (x,cycles) value after finishing execution
-- noop instructions leave x unchanged and take one cycle
-- addx instructions add some value to x and take two cycles
processInstruction :: (Int, Int) -> String -> (Int, Int)
processInstruction (x,c) instruction
    | instruction == "noop"         = (x,c+1)
    | isPrefixOf "addx" instruction = (x+n,c+2)
    | otherwise                     = (x,c)
    where n = read $ (last . words) instruction
    
-- finds the (x,c) value at a certain cycle number 
-- by returning the last (x,c) value where c < cycle
stateAt :: [(Int, Int)] -> Int -> (Int, Int)
stateAt states cycle = last $ takeWhile (\x -> snd x < cycle) states

-- decides whether to draw a pixel to the screen given the current x register and the cycle
-- x register controls the horizontal position of a 3-wide strip of pixels
-- each row is 40 pixels long
-- if the current value of cycle is within this strip, draw a pixel to the screen
shouldDraw :: Int -> Int -> Bool
shouldDraw c x = elem (mod c 40) [x-1..x+1]

-- gets some row of the output
-- rows are 40 pixels wide
row :: String -> Int -> String
row pixels n = take 40 $ drop (40*n) pixels

solve :: IO ()
solve = do
    input <- readFile "input/Day10.txt"
    
    
    -- (x, cycles) starts at (1,0)
    let states = scanl processInstruction (1,0) (lines input)
        
        -- puzzle 1 is to find the signal strength at [20,60..220]
        -- signal strength is found by multiplying x by the cycle count
        signals = map (stateAt states) [20,60..220]
        signalStrengths = zipWith (*) (map fst signals) [20,60..220]
        
        
        -- fills in the gaps of 2-cycle instructions to get the state at each and every cycle
        fullStates = foldr (\x acc -> if elem (snd x + 1) (map snd acc) then x:acc else x:(fst x, snd x+1):acc) [] states
        
        -- orders the (x,cycle) values by cycle number
        toDraw = sortBy (comparing snd) fullStates
        picture = map (\(x,c) -> if shouldDraw c x then '#' else ' ') toDraw
        -- puzzle description uses # and . for drawn/undrawn but spaces make it clearer
        
        -- break it up into 40-width lines to print cleanly
        pictureRows = map (row picture) [0..5]
        
    putStrLn $ "First  Solution:\t" ++ show(sum signalStrengths)
    putStrLn $ "Second Solution:"
    putStrLn (unlines pictureRows)