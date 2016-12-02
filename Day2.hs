
import           Data.List
import           Data.List.Split

-- data types

type Steps = Int
type Position = (Int,Int)

-- take one step int the given direction
move 'U' (x,y) = (x+0,y-1)
move 'R' (x,y) = (x+1,y+0)
move 'D' (x,y) = (x+0,y+1)
move 'L' (x,y) = (x-1,y+0)

-- only perform move when move valid
validMove1 d p = let (x',y') = move d p in (min 2 (max 0 x'), min 2 (max 0 y'))
validMove2 d p = let p' = move d p in if cbd (2,2) p' < 3 then p' else p

-- city block distance
cbd (ax,ay) (bx,by) = abs (bx-ax) + abs (by-ay)

-- calculate key from 
key1 (x,y) = show $ x + y * 3 + 1
key2 (x,y) = "XX1XXX234X56789XABCXXXDXX" !! (x + y * 5)

-- go go go
main = do
    -- read and parse instructions
    instructions <- lines <$> readFile "input-2-1.txt"

    -- follow instructions and remember positions
    let positions1 = tail $ scanl' (foldl' (flip validMove1)) (1,1) instructions
        positions2 = tail $ scanl' (foldl' (flip validMove2)) (0,2) instructions

    -- code
    let code1 = concatMap key1 positions1
        code2 = map key2 positions2


    -- show results
    putStrLn $ "result part one: " ++ show code1
    putStrLn $ "result part two: " ++ show code2
