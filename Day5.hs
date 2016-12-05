import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as BS

import Data.List
import qualified Data.IntMap.Strict as Map

make  n key i = show . md5 $ BS.pack (key ++ show i)
check = ("00000" `isPrefixOf`)

check2 cs = (cs !! 5) `elem` "01234567"

updateKey :: Map.IntMap Char -> String -> Map.IntMap Char
updateKey m c = Map.insertWith (\_ b -> b) (read [c !! 5]) (c !! 6) m

pass2 :: Map.IntMap Char -> String
pass2 m = Map.elems $ Map.union m (Map.fromList (zip [0..] "________"))

main = do

    let key = "abbhdwsy"
    {-let key = "abc"-}

    let hashes = filter check . map (make 5 key) $ [0..]

    let result1 = map (!!5) . take 8 $ hashes

    let hashes2 = filter check2 hashes

    let result2 = map pass2
                . takeWhile (\m -> Map.size m <= 8) 
                . scanl' updateKey Map.empty
                $ hashes2

    putStrLn $ "result part one: " ++ show result1
    putStrLn $ "result part two: "
    mapM_ print result2
