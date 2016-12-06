
import Data.List

import qualified Data.Map.Strict as Map

bestChars s = map fst $ reverse $ sortOn snd $ Map.toList $ Map.fromListWith (+) (zip s (repeat 1))

main = do

    i <- lines <$> readFile "input-6-1.txt"
    let bs = map bestChars (transpose i)

    putStrLn $ "result part one: " ++ map head bs
    putStrLn $ "result part one: " ++ map last bs
