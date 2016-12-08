

import Text.Megaparsec
import Text.Megaparsec.String



-- | matrix is defined by width, height and a function that returns the
-- state at a given x y positions
data Mat = Mat { width :: Int, height :: Int, index :: Int -> Int -> Bool}

matShow (Mat w h f) = unlines [[ if f x y then '#' else ' ' | x <- [0..w-1]] | y <- [0..h-1]]

empty w h = Mat w h (\x y -> False)
rect w h m = m { index = \x y -> ((x < w && y < h) || index m  x y) }
rotCol c a m = m { index = \x y -> index m x (if x == c then (y - a) `mod` height m else y) }
rotRow r a m = m { index = \x y -> index m (if y == r then (x - a) `mod` width m else x) y }


-- parser for instruction file
instructions :: Parser [Mat -> Mat]
instructions = (rectP <|> rotRowP <|> rotColP) `sepEndBy1` newline
rectP = rect <$> (string "rect " *> intP) <*> (string "x" *> intP)
rotRowP = rotRow <$> (string "rotate row y=" *> intP) <*> (string " by " *> intP)
rotColP = rotCol <$> (string "rotate column x=" *> intP) <*> (string " by " *> intP)
intP = read <$> some numberChar

-- run!
main = do

    let fn = "input-8-1.txt"
    (Right is) <- parse instructions fn <$> readFile fn

    let finalMat = foldl (\m f -> f m) (empty 50 6) is
        result1  = length . filter (=='#') $ result2
        result2  = matShow finalMat

    putStrLn $ "result part one: " ++ show result1
    putStrLn $ "result part two: \n" ++ result2
