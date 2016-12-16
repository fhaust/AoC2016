
{-# LANGUAGE ViewPatterns #-}

import qualified Data.Sequence as Seq
import           Data.Monoid
import           Data.Foldable

input = readCurve "11100010111110100"
{-inputLen = 272-}
inputLen = 35651584

step c = c <> [False] <> (reverse . map not $ c)

showCurve :: [Bool] -> String
showCurve = fmap (\c -> if c then '1' else '0')

readCurve :: String -> [Bool]
readCurve = map (/= '0')

curves = iterate step input


curveLen l = take l <$> find ((>= l) . length) curves


checksum :: [Bool] -> [Bool]
checksum s = if even (length staging) then checksum staging else staging
    where go (a:b:rst) | a == b    = True  : go rst
                       | otherwise = False : go rst
          go []        =             []
          staging      = go s


main = do

    let input = readCurve "11100010111110100"
        len1  = 272
        len2  = 35651584

        (Just result1) = showCurve . checksum <$> curveLen len1
        (Just result2) = showCurve . checksum <$> curveLen len2

    putStrLn $ "result part one: " ++ show result1
    putStrLn $ "result part two: " ++ show result2
