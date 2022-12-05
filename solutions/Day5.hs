module Day5 where

import Data.List

type Stack = String
type Dock = [Stack]


moveCrate :: (Stack, Stack) -> (Stack, Stack)
moveCrate (first, second) = (tail first, box:second)
    where box = head first
    
moveCrate9001 :: Int -> (Stack, Stack) -> (Stack, Stack)
moveCrate9001 num (first, second) = (drop num first, moved++second)
    where moved = take num first
    

    
operateCrane :: Dock -> (Int, Int, Int) -> Dock
operateCrane dock (num, first, second)
    | first < second = initial++newFirst:middle++newSecond:final
    | second < first = initial++newSecond:middle++newFirst:final
    where numBefore = (min first second) - 1
          numBtween = (max first second) - (min first second) -1
          numAfter  = 9 - (max first second)
          
          initial = take numBefore dock
          middle  = take numBtween $ drop (numBefore+1) dock
          final   = drop (numBefore+numBtween+2) dock
          
          (newFirst, newSecond) = iterate (moveCrate) (dock !! (first-1), dock !! (second-1)) !! num
  
operateCrane9001 :: Dock -> (Int, Int, Int) -> Dock
operateCrane9001 dock (num, first, second)
    | first < second = initial++newFirst:middle++newSecond:final
    | second < first = initial++newSecond:middle++newFirst:final
    where numBefore = (min first second) - 1
          numBtween = (max first second) - (min first second) - 1
          numAfter = 9 - (max first second)
          
          initial = take numBefore dock
          middle  = take numBtween $ drop (numBefore+1) dock
          final   = drop (numBefore+numBtween+2) dock
          
          (newFirst, newSecond) = moveCrate9001 num (dock !! (first-1), dock !! (second-1))
  
parseInstruction :: String -> (Int, Int, Int)
parseInstruction instruction = (read num, read first, read second)
    where num    = (words instruction) !! 1
          first  = (words instruction) !! 3
          second = (words instruction) !! 5


parseCrane :: Dock -> String -> Dock
parseCrane dock string = operateCrane dock (parseInstruction string)       

parseCrane9001 :: Dock -> String -> Dock
parseCrane9001 dock string = operateCrane9001 dock (parseInstruction string)

solve :: IO ()
solve = do
    input <- readFile "input/Day5.txt"
    
    let instructions = drop 10 (lines input)
        initialDock = ["FGVRJLD","SJHVBMPT","CPGDFMHV","QGNPDM","FNHLJ","ZTGDQVFN","LBDF","NDVSBJM","DLG"] 
        
        finalDock = foldl parseCrane initialDock instructions
        finalTops = map head finalDock
        
        finalDock9001 = foldl parseCrane9001 initialDock instructions
        finalTops9001 = map head finalDock9001
    
    putStrLn $ show finalTops
    putStrLn $ show finalTops9001