


import Text.Megaparsec
import Text.Megaparsec.String

import qualified Data.Map.Strict as Map

import Data.List

-- | parser for room descriptors
room :: Parser (String,Int,String)
room = (,,) <$> name <*> sid <*> checksum
  where name = concat <$> (many lowerChar `sepBy` char '-')
        sid = read <$> some numberChar
        checksum = char '[' >> some lowerChar <* char ']'

-- | calculate checksum from room name
mkChecksum s = take 5 . map fst . sortBy (\a b -> compare (snd b) (snd a))
             . Map.toList $ Map.fromListWith (+) (zip s (repeat 1))

-- | decrypt room description
decrypt (n,i,c) = (iterate step n !! i, i, c)
  where step = map (\c -> if c == 'z' then 'a' else toEnum (fromEnum c + 1))


main = do

    let fn = "input-4-1.txt"
    (Right rooms) <- parse (room `sepEndBy` newline) fn <$> readFile fn

    let validRooms = filter (\(n,_,c) -> mkChecksum n == c) rooms
    let result1 = sum . map (\(_,i,_) -> i) $ validRooms

    let decryptedRooms = map decrypt validRooms
    let (Just (_,result2,_)) = find (\(n,i,c) -> "pole" `isInfixOf` n) decryptedRooms

    putStrLn $ "result part one: " ++ show result1
    putStrLn $ "result part two: " ++ show result2


