import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as BS

import qualified Data.IntMap.Strict as Map

import Data.List
import Data.Hex

make  n key i = hex . MD5.hash $ BS.pack (key ++ show i)

check     = (BS.pack "00000" `BS.isPrefixOf`)
check2 cs = BS.index cs 5 `elem` "01234567"

updateKey m c = Map.insertWith (\_ b -> b) (read [BS.index c 5]) (BS.index c 6) m

main = do

    let key = "abbhdwsy"

    let hashes = filter check . map (make 5 key) $ [0..]

    let result1 = map (`BS.index` 5) . take 8 $ hashes

    let hashes2 = filter check2 hashes

    let result2 = Map.elems . head
                . dropWhile (\m -> Map.size m < 8)
                . scanl' updateKey Map.empty
                $ hashes2

    putStrLn $ "result part one: " ++ show result1
    putStrLn $ "result part two: " ++ show result2
