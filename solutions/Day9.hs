module Day9 where

import Data.List

type Position = (Int, Int)
type Rope = (Position, Position)
type LinkedRope = (Position, [Position])
type Motion = (String, Int)

-- gets displacement between two positions
(~-) :: Position -> Position -> Position
(x1,y1) ~- (x2,y2) = (x1-x2,y1-y2)


-- moves head one step in a direction          
updateHead :: String -> Rope -> Rope
updateHead dir (h@(hx,hy), t) 
    | dir == "R" = ((hx+1,hy),t)
    | dir == "L" = ((hx-1,hy),t)
    | dir == "D" = ((hx,hy-1),t)
    | dir == "U" = ((hx,hy+1),t)
    | otherwise  = (h,t)
    
-- updates a tail to keep up with a head
-- tails move in both directions towards the head if they are 3 or more units away
-- if they are 2 away in either direction, they move towards the head in that directions
-- otherwise, they stay put
updateTail :: Rope -> Rope
updateTail (h, t@(tx,ty))
    | dist    >= 3 = (h, (tx+signum(dx), ty+signum(dy)))
    | abs(dx) >= 2 = (h, (tx+signum(dx), ty))
    | abs(dy) >= 2 = (h, (tx, ty+signum(dy)))
    | otherwise    = (h,t)
    where (dx, dy) = h ~- t 
          dist = abs(dx) + abs(dy)
 
-- given a motion (dir, num), returns the position of the (head,tail) having moved dir num times
-- we actually break up the input into single-movement items here, so some functionality goes unused 
moveRope :: Rope -> Motion -> Rope
moveRope rope (dir, num) = iterate (updateTail . updateHead dir) rope  !! num


-- functions for handling a linkedrope


-- function for adjusting the leader of a rope
-- uses the rules for updateHead above
updateLeader :: String -> Position -> Position
updateLeader s h = fst $ updateHead s (h,(0,0))
          
-- function for adjusting a single link of a rope     
-- uses the rules for updateTail above     
updateLink :: Position -> Position -> Position
updateLink h t = snd $ updateTail(h,t)

-- here, we update each link using the previous link as its "leader"
updateTrail :: Position -> [Position] -> [Position]
updateTrail h t = tail $ scanl updateLink h t
          

-- combines the above functions for use with scanl
moveLinkedRope :: LinkedRope -> Motion -> LinkedRope
moveLinkedRope lrope@(h,t) (dir,num) = (newHead, newTrail)
    where newHead = updateLeader dir h
          newTrail = updateTrail newHead t



-- parses lines like "R 3" into motions like ("R", 3)
parseMove :: String -> Motion
parseMove move = ((head . words) move, read $ (last . words) move)


-- breaks an instruction like "R 3" into 3 "R 1" instructions
breakMove :: String -> String
breakMove move = unlines $ take num (repeat $ dir ++ " 1")
    where num = read $ (last . words) move
          dir = (head . words) move



solve :: IO ()
solve = do
    input <- readFile "input/Day9.txt"

    -- read the input and break it into single-step moves
    let moveStrings = (lines input)
        brokenMoves = (lines . concat) (map breakMove moveStrings)
        
        moves = map parseMove brokenMoves
        
        -- puzzle one concerns a standard (head, tail) rope
        head = (0,0)
        tail = (0,0)
        -- puzzle two has a longer rope with a head and 9 links
        linkedRope = (head, take 9 (repeat (0,0)))
        
        positions = scanl moveRope (head,tail) moves
        tailPositions = map snd positions
        
        linkedPositions = scanl moveLinkedRope linkedRope moves
        linkedTailPositions = map (last . snd) linkedPositions
        
        numVisited = (length . nub) tailPositions 
        numLinkedVisited = (length . nub) linkedTailPositions
    
    
    putStrLn $ "First  Solution:\t" ++ show numVisited
    putStrLn $ "Second Solution:\t" ++ show numLinkedVisited