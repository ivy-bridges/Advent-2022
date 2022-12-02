module Day2 where

data Shape = Rock | Paper | Scissors
    deriving (Read, Show, Eq, Ord)

-- defining an alternate ordering that loops around
-- A > B means that A beats B
sCompare :: Shape -> Shape -> Ordering
sCompare Scissors Rock = LT
sCompare Rock Scissors = GT
sCompare a b = compare a b
-- defaults to compare when not needing to wrap
-- defining an operator for ease of use
(?=) = sCompare






-- points given by choice of move
selectScore :: Shape -> Int
selectScore Rock = 1
selectScore Paper = 2
selectScore Scissors = 3

-- points given by result of match
outcomeScore :: Ordering -> Int
outcomeScore LT = 0
outcomeScore EQ = 3
outcomeScore GT = 6

-- converting from the guide to a choice of moves
toShape :: Char -> Shape
toShape 'A' = Rock
toShape 'B' = Paper
toShape 'C' = Scissors

toShape 'X' = Rock
toShape 'Y' = Paper
toShape 'Z' = Scissors 


-- total points for one single game, following the first guide
-- just adds points for move and points for outcome
gamePoints :: String -> Int
gamePoints game = selectScore choice + outcomeScore (choice ?= opp)
    where opp = toShape (head game)
          choice = toShape (last game)
          
-- rule for following the second guide
toOutcome :: Char -> Ordering
toOutcome 'X' = LT
toOutcome 'Y' = EQ
toOutcome 'Z' = GT
          
-- we don't actually need to figure out our move
-- we just need the points for the outcome
-- and to shift the points for selection up or down a bit
riggedPoints :: String -> Int
riggedPoints game 
    | outcome == LT = 0 + (oppPoints - 2) `mod` 3 + 1
    | outcome == EQ = 3 + (oppPoints)
    | outcome == GT = 6 + (oppPoints) `mod` 3 + 1
    where opp       = toShape (head game)
          outcome   = toOutcome (last game)
          oppPoints = selectScore opp
          
          


solve :: IO ()
solve = do
    contents <- readFile "input/Day2.txt"
    
    let rounds = lines contents
        normalScore = sum $ map gamePoints  rounds
        riggedScore = sum $ map riggedPoints rounds
    
    putStrLn $ "First  Solution:\t" ++ show normalScore
    putStrLn $ "Second Solution:\t" ++ show riggedScore
    