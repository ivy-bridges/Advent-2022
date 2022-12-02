module Day2 where

data Shape = Rock | Paper | Scissors
    deriving (Read, Show, Eq)


result :: Shape -> Shape -> Int
result Rock Scissors = 6
result Rock Paper = 0
result Rock Rock = 3
result Scissors Scissors = 3
result Paper Paper = 3
result Scissors Rock = 0
result Scissors Paper = 6
result Paper Rock = 6
result Paper Scissors = 0








select :: Shape -> Int
select Rock = 1
select Paper = 2
select Scissors = 3


toShape :: Char -> Shape
toShape 'A' = Rock
toShape 'X' = Rock
toShape 'B' = Paper
toShape 'Y' = Paper
toShape 'C' = Scissors
toShape 'Z' = Scissors 

roundPoints :: String -> Int
roundPoints info = (select choice) + (result choice opp)
    where opp = toShape (head info)
          choice = toShape (last info)
          
          
losesTo :: Shape -> Shape
losesTo Rock = Scissors
losesTo Scissors = Paper
losesTo Paper = Rock
  

beats :: Shape -> Shape
beats Rock = Paper
beats Scissors = Rock
beats Paper = Scissors

ties :: Shape -> Shape
ties s = s

rule :: Char -> (Shape -> Shape)
rule 'X' = losesTo
rule 'Y' = ties
rule 'Z' = beats

riggedPoints :: String -> Int
riggedPoints info = (select choice) + (result choice opp)
    where opp = toShape (head info)
          choice = rule (last info) $ opp
          


solve :: IO ()
solve = do
    contents <- readFile "input/Day2.txt"
    
    let rounds = lines contents
        normalScore = sum $ map roundPoints  rounds
        riggedScore = sum $ map riggedPoints rounds
    
    putStrLn $ "First  Solution:\t" ++ show normalScore
    putStrLn $ "Second Solution:\t" ++ show riggedScore
    