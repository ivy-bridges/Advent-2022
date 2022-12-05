module Day5 where

import Data.List

type Stack = String
type Dock = [Stack]

-- a crane move takes a number of crates to moves, and two stacks
-- and returns the two stacks with that number of crates moved
type CraneMove = (Int -> (Stack, Stack) -> (Stack, Stack))


-- given a set of stacks, like
-- [N]    
-- [Z] [M] [P]
-- returns ["NZ","M","P"]
readInitialDock :: [String] -> Dock
readInitialDock str = map (dropWhile (==' ')) stacks
    where stacks  = filter (\c -> last c `elem` ['A'..'Z']) columns
          columns = transpose str


-- moves a single crate according to the rules of CrateMover 9000
moveCrate :: (Stack, Stack) -> (Stack, Stack)
moveCrate (first, second) = (tail first, box:second)
    where box = head first
    
-- move multiple crates according to the rules of CrateMover 9000
moveCrates :: CraneMove
moveCrates num (first, second) = iterate moveCrate (first, second) !! num

-- move multiple crates according to the rules of CrateMover 9001
moveCrates9001 :: CraneMove
moveCrates9001 num (first, second) = (drop num first, moved++second)
    where moved = take num first
    
-- takes a dock, an instruction, and a crane move
-- and outputs the dock when the crates are moved according to the crane's rules for moving
-- does this by breaking the dock into chunks, performing the crane's move, and putting it back together
modifyDock :: Dock -> (Int, Int, Int) -> CraneMove -> Dock
modifyDock dock (num, first, second) moveType
    | first < second = initial++newFirst:middle++newSecond:final
    | second < first = initial++newSecond:middle++newFirst:final
    where numBefore = (min first second) - 1
          numBtween = (max first second) - (min first second) -1
          
          initial = take numBefore dock
          middle  = take numBtween $ drop (numBefore+1) dock
          final   = drop (numBefore+numBtween+2) dock
          
          -- swaps the crates according to the crane's move
          (newFirst, newSecond) = moveType num (dock !! (first-1), dock !! (second-1))
    

-- given a string of form "move n from a to b"
-- returns (n, a, b)  
parseInstruction :: String -> (Int, Int, Int)
parseInstruction instruction = (read num, read first, read second)
    where num    = (words instruction) !! 1
          first  = (words instruction) !! 3
          second = (words instruction) !! 5


-- some combinations of the above functions for use with foldl
parseCrane :: Dock -> String -> Dock
parseCrane dock string = modifyDock dock (parseInstruction string) moveCrates   

parseCrane9001 :: Dock -> String -> Dock
parseCrane9001 dock string = modifyDock dock (parseInstruction string) moveCrates9001

solve :: IO ()
solve = do
    input <- readFile "input/Day5.txt"
    
    let dockInfo     = takeWhile (not . elem '1') (lines input)
        instructions = dropWhile (not . elem 'm') (lines input)
        
        initialDock  = readInitialDock dockInfo
        
        finalDock = foldl parseCrane initialDock instructions
        finalTops = map head finalDock
        
        finalDock9001 = foldl parseCrane9001 initialDock instructions
        finalTops9001 = map head finalDock9001
    
    putStrLn $ "First  Solution:\t" ++ show finalTops
    putStrLn $ "Second Solution:\t" ++ show finalTops9001