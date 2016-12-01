
import           Data.List
import           Data.List.Split

-- data types

data Heading = N | E | S | W deriving (Show,Eq)
data Direction = L | R deriving (Show,Read,Eq)
type Steps = Int
type Position = (Int,Int)
data Input = Input Direction Steps deriving (Show,Eq)

-- direction resulting from turning left or right
turn R N = E
turn R E = S
turn R S = W
turn R W = N
turn L N = W
turn L W = S
turn L S = E
turn L E = N

-- take one step int the given direction
move _ 0 (x,y) = (x,y) : []
move N s (x,y) = (x,y) : move N (s-1) (x+0,y+1)
move E s (x,y) = (x,y) : move E (s-1) (x+1,y+0)
move S s (x,y) = (x,y) : move S (s-1) (x+0,y-1)
move W s (x,y) = (x,y) : move W (s-1) (x-1,y+0)

-- code to parse input file
parseInstructions = map (\(d:i) -> Input (read [d]) (read i)) . splitOn ", " . init

-- actual movement
followInstructions (h,ps) (Input d s) = let h' = turn d h
                                            ps' = (init ps) ++ move h' s (last ps)
                                        in (h',ps')

-- city block distance
cbDistance (ax,ay) (bx,by) = abs (bx-ax) + abs (by-ay)

-- find first positions visited twice
firstTwice ps = head (ps \\ nub ps)

-- go go go
main = do
    -- read and parse instructions
    instructions <- parseInstructions <$> readFile "input-1-1.txt"

    -- follow instructions and remember positions
    let positions = snd . foldl followInstructions (N,[(0,0)]) $ instructions

    -- distance
    let distance = cbDistance (last positions) (0,0)

    -- first position visited twice
    let hqPosition2 = firstTwice positions
        distance2   = cbDistance hqPosition2 (0,0)

    -- show results
    putStrLn $ "result part one: " ++ show distance
    putStrLn $ "result part two: " ++ show distance2


