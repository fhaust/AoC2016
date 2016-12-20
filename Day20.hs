import Data.List
import Data.List.Split

import qualified Data.RangeSet.IntMap as S

parseInput = map go . lines
    where go s = let [a,b] = splitOn "-" s in (read a, read b)

main = do

    ls <- parseInput <$> readFile "input-20-1.txt"

    let all     = S.singletonRange (0,4294967295)
        invalid = S.fromRangeList ls
        valid   = S.difference all invalid

    let result1 = S.findMin valid
        result2 = S.size valid

    putStrLn $ "result part one: " ++ show result1
    putStrLn $ "result part two: " ++ show result2
