module Day8 where

import Data.List
import Data.Char

type TreeGrid = String
type Coordinate = (Int, Int)


-- returns a list of lines of trees between a coordinate and the edge of the grid
-- structured as = [ trees to the left, trees to the right, trees above, trees below ]
-- lines of trees are ordered by distance from the coordinate (so closest trees appear first)
competitors :: TreeGrid -> Coordinate -> [[Int]]
competitors grid (x,y) = [reverse $ take x row, drop (x+1) row, reverse $ take y col, drop (y+1) col]
    where row = map digitToInt $ lines grid !! y
          col = map digitToInt $ (transpose . lines) grid !! x
          
          
-- returns whether a tree at (x,y) is visible on some grid
-- trees on the boundary of a grid (on the first or last row, or on the first or last column) are always visible
-- trees not on the boundary check if there exists a sight line where they are taller than all of the trees in it
isVisible :: TreeGrid -> Coordinate -> Bool
isVisible grid (x,y)
    | onBoundary = True 
    | otherwise   = any (<treeHeight) maxLines 
    where treeLines = competitors grid (x,y)
          maxLines  = map maximum treeLines
          treeHeight = digitToInt $ (lines grid) !! y !! x
          
          onBoundary = ( (x==0) || (y==0) ) || ( (y+1==length(lines grid)) || (x+1==length(lines grid !! 0)) ) 

-- gets a list of (x,y) values from a rectangular string
coordinates :: TreeGrid -> [Coordinate]
coordinates grid = [(x,y) | x <- xs, y <- ys]
    where xs = [0..width -1]
          ys = [0..height-1]
          
          width = length (lines grid !! 0)
          height = length (lines grid) 


-- given a predicate and a list
-- returns the list up until and including when the predicate is met
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs)
    | p x = [x]
    | otherwise = x:(takeUntil p xs)

-- gets the scenic score of some tree in our grid
-- scenic score is found by taking the number of trees until it gets blocked in each direction
-- and multiplying them together
scenicScore :: TreeGrid -> Coordinate -> Int
scenicScore grid (x,y) = product (map length sightlines)
    where treelines = competitors grid (x,y)
          treeheight = digitToInt $ (lines grid) !! y !! x
          
          sightlines = map (takeUntil (>=treeheight)) treelines




solve :: IO ()
solve = do
    forestGrid <- readFile "input/Day8.txt"
    
    let visibleTrees = filter (isVisible forestGrid) (coordinates forestGrid)
    
        views = map (competitors forestGrid) (coordinates forestGrid)
        
        scenicScores = map (scenicScore forestGrid) (coordinates forestGrid)
  
        
    
    putStrLn $ "First  Solution:\t" ++ show (length visibleTrees)
    putStrLn $ "Second Soltuion:\t" ++ show (maximum scenicScores)
    
    