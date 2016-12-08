

import Text.Megaparsec
import Text.Megaparsec.String

-- NOTE intentionally mystified

-- | matrix is defined by width, height and a function that returns the
-- state at a given x y positions
type Mat = Int -> Int -> Bool

matShow f = unlines [[ if f x y then '#' else ' ' | x <- [0..50-1]] | y <- [0..6-1]]

empty        x y = False
rect   w h f x y = (x < w && y < h) || f x y
rotCol c a f x y = let y' = if x == c then (y - a) `mod` 6 else y in f x  y'
rotRow r a f x y = let x' = if y == r then (x - a) `mod` 50 else x in f x' y


-- parser for instruction file
instructions :: Parser [Mat -> Mat]
instructions = (rectP <|> rotRowP <|> rotColP) `sepEndBy1` newline
rectP        = rect <$> (string "rect " *> intP) <*> (string "x" *> intP)
rotRowP      = rotRow <$> (string "rotate row y=" *> intP) <*> (string " by " *> intP)
rotColP      = rotCol <$> (string "rotate column x=" *> intP) <*> (string " by " *> intP)
intP         = read <$> some numberChar

-- run!
main = do

    let fn = "input-8-1.txt"
    (Right is) <- parse instructions fn <$> readFile fn

    let finalMat = foldl (\m f -> f m) empty is
        result1  = length . filter (=='#') $ result2
        result2  = matShow finalMat

    putStrLn $ "result part one: " ++ show result1
    putStrLn $ "result part two: \n" ++ result2
