


import Data.List.Split

-- | splits a list on even and uneven positions
splitr (a:b:rst) = let (as,bs) = splitr rst in (a:as, b:bs)
splitr [a]       = ([a],[])


hasABBA (a:b:c:d:rst) = (a == d && b == c && a /= b) || hasABBA (b:c:d:rst)
hasABBA _             = False


hasTLS l = any hasABBA a && all (not . hasABBA) b
    where (a,b) = splitr . splitOneOf "[]" $ l


extractABAs (a:b:c:rst) | a == c     = [a,b,c] : extractABA (b:c:rst)
                        | otherwise  = extractABA (b:c:rst)
extractABAs _                        = []


hasSSL l = any (`elem` babs) abas
    where (a,b) = splitr . splitOneOf "[]" $ l
          abas  = concatMap extractABAs a
          babs  = map (\[a,b,c] -> [b,a,b]) . concatMap extractABAs $ b


main = do

    ls <- lines <$> readFile "input-7-1.txt"

    let result1 = length . filter hasTLS $ ls
    let result2 = length . filter hasSSL $ ls

    putStrLn $ "result part one: " ++ show result1
    putStrLn $ "result part two: " ++ show result2
