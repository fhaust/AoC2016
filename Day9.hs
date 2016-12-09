module Main (main) where

import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import           Data.Foldable

-- part 1: decompress without recursing into the compressed sub parts

decompress1 :: Seq Char -> Seq Char
decompress1 s | Seq.null rst1 = init
              | otherwise     = mconcat [init, mconcat . replicate rep $ sub, decompress1 rst3]
    where (init,rep,sub,rst1,rst3) = breakString s

-- part 2: same thing, but this time we recurse to the subs

decompress2 :: Seq Char -> Seq Char
decompress2 s | Seq.null rst1 = init
              | otherwise     = mconcat [init, mconcat . replicate rep . decompress2 $ sub, decompress2 rst3]
    where (init,rep,sub,rst1,rst3) = breakString s


-- Data.Sequence is not meant to be a string thing, matching and breaking 
-- looks kind of awkward ... 

breakString :: Seq Char -> (Seq Char, Int, Seq Char, Seq Char, Seq Char)
breakString s = (init,rep,sub,rst1,rst3)
    where (init,rst1) = Seq.breakl (=='(') s
          (nums,rst2) = Seq.breakl (==')') rst1
          (len,rep)   = parseNums nums
          (sub,rst3)  = Seq.splitAt len . Seq.drop 1 $ rst2

-- same thing here ... reading ints from Data.Sequence by converting to
-- list again

parseNums :: Seq Char -> (Int,Int)
parseNums s = (read . toList $ a,read . toList $ Seq.drop 1 b)
    where (a,b) = Seq.breakl (=='x') . Seq.drop 1 $ s

main = do

    -- read inputfile as Sequence (discard newline at the end)
    ls <- Seq.fromList . head . lines <$> readFile "input-9-1.txt"

    -- calculate lenghts of decompressed file
    let result1 = Seq.length . decompress1 $ ls
        result2 = Seq.length . decompress2 $ ls

    putStrLn $ "result part one: " ++ show result1
    putStrLn $ "result part two: " ++ show result2

