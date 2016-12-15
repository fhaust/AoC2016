



discFilter index positions start = filter go
    where go t = ((start + t + index) `mod` positions) == 0


parseInstruction :: String -> ([Int] -> [Int])
parseInstruction s = discFilter (read (tail (ws !! 1))) (read (ws !! 3)) (read (init (ws !! 11)))
    where ws = words s

main = do

    is <- map parseInstruction . lines <$> readFile "input-15-1.txt"

    let result1 = head $ foldl (\t f -> f t) [0..] is
    let result2 = head $ foldl (\t f -> f t) [0..] (is ++ [discFilter 7 11 0])

    putStrLn $ "result part one: " ++ show result1
    putStrLn $ "result part two: " ++ show result2
