parseInput = map (map read . words) . lines

mangleInput []                            = []
mangleInput ([a,b,c]:[d,e,f]:[h,i,j]:rst) = [a,d,h]:[b,e,i]:[c,f,j] : mangleInput rst

valid [a,b,c] | a + b <= c || a + c <= b || b + c <= a = False
              | otherwise  = True

main = do
    is <- parseInput <$> readFile "input-3-1.txt"
    let c = length $ filter valid is

    putStrLn $ "result part one: " ++ show c

    let is' = mangleInput is
        c'  = length $ filter valid is'

    putStrLn $ "result part two: " ++ show c'
