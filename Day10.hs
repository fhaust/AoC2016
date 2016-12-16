

import           Data.List

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map

import           Debug.Trace

processValues s m | head ws == "value" = addValue (read (ws !! 5)) (read (ws !! 1)) m
                  | otherwise          = m -- error "invalid instruction"
    where ws = words s


addValue :: Int -> Int -> IntMap [Int] -> IntMap [Int]
addValue bot v = Map.alter go bot
    where go (Just vs) = Just (v:vs)
          go Nothing   = Just [v]

processInstructions :: String -> IntMap (Int,Int) -> IntMap (Int,Int)
processInstructions s m | head ws == "bot" = Map.insert (read (ws !! 1)) (low,high) m
                        | otherwise        = m
    where ws = words s
          low  = let v = read (ws !! 6) in if ws !! 5 == "output" then (v-100) else v
          high = let v = read (ws !! 11) in if ws !! 10 == "output" then (v-100) else v


step is s = Map.foldlWithKey' go s givers
    where givers = Map.filter (\vs -> length vs > 1) s
          go m bot vs = (trace $ "bot " ++ show bot ++ " compares " ++ show lowValue ++ " to " ++ show highValue)
                      . Map.insert bot [] 
                      . addValue highBot highValue
                      . addValue lowBot  lowValue
                      $ m
                      where (lowBot,highBot) = is Map.! bot
                            [lowValue,highValue] = sort vs


test = do

    ls <- lines <$> readFile "input-10-1.txt"

    let initState    = foldl' (flip processValues) Map.empty ls
        instructions = foldl' (flip processInstructions) Map.empty ls

    return (initState,instructions)
