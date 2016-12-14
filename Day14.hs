


import qualified Crypto.Hash as H
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteArray.Encoding as BA

import Data.List
import Data.Maybe


hashes :: String -> [BS.ByteString]
hashes key = [ md5 $ BS.pack (key ++ show i) | i <- [0..] ]

stretchedHashes :: String -> [BS.ByteString]
stretchedHashes key = [ iterate md5 (BS.pack (key ++ show i)) !! (2016+1) | i <- [0..] ]

hasTriplet :: String -> Maybe Char
hasTriplet (a:b:c:d:rst) | a == b && a == c && a /= d = Just a
                         | otherwise                  = hasTriplet (b:c:d:rst)
hasTriplet [a,b,c]       | a == b && a == c           = Just a
                         | otherwise                  = Nothing

hasTriplet :: String -> Maybe Char
hasFiver (a:b:c:d:e:f:rst) | a == b && a == c && a == d && a == e && a /= f = Just a
                           | otherwise                                      = hasFiver (b:c:d:e:f:rst)
hasFiver [a,b,c,d,e]       | a == b && a == c && a == d && a == e           = Just a
                           | otherwise                                      = Nothing

md5 :: BS.ByteString -> BS.ByteString
md5 = BA.convertToBase BA.Base16 . (H.hash :: BS.ByteString -> H.Digest H.MD5)


search64thHash :: [BS.ByteString] -> Int
search64thHash hashes = is !! (64-1)
    where ts = map (hasTriplet . BS.unpack) hashes
          fs = map (hasFiver   . BS.unpack) hashes
          rs = zip ts (map (take 1000) . tail . tails $ fs)
          is = findIndices go rs
          go (Just a, bs) = a `elem` catMaybes bs
          go _            = False

main = do

    let hashes1 = hashes "zpqevtbw"
        result1 = search64thHash hashes1
        hashes2 = stretchedHashes "zpqevtbw"
        result2 = search64thHash hashes2

    putStrLn $ "result part one: " ++ show result1
    putStrLn $ "result part two: " ++ show result2
