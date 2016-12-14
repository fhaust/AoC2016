

import           Text.Megaparsec
import           Text.Megaparsec.String

import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as VUM


data Register = A | B | C | D deriving (Show,Eq,Enum)

data Instruction = CpyVal Int      Register
                 | CpyReg Register Register
                 | Inc    Register
                 | Dec    Register
                 | JnzReg Register Int
                 | JnzVal Int      Int
                 deriving (Show,Eq)


instructionsP :: Parser (V.Vector Instruction)
instructionsP = V.fromList <$> instructionP `sepEndBy` newline

instructionP = try cpyValP <|> try cpyRegP <|> try incP <|> try decP <|> try jnzRegP <|> try jnzValP
cpyValP = CpyVal <$> (string "cpy " *> intP) <*> (space *> registerP)
cpyRegP = CpyReg <$> (string "cpy " *> registerP) <*> (space *> registerP)
incP    = Inc    <$> (string "inc " *> registerP)
decP    = Dec    <$> (string "dec " *> registerP)
jnzRegP = JnzReg <$> (string "jnz " *> registerP) <*> (space *> intP)
jnzValP = JnzVal <$> (string "jnz " *> intP) <*> (space *> intP)

-- | parse an integer
-- not exactly correct because it would try to parse something like
-- "123-123" as an integer and die horribly
intP :: Parser Int
intP = read <$> some (char '-' <|> numberChar)

registerP :: Parser Register
registerP = do
    c <- oneOf ("abcd" :: String)
    return $ case c of
      'a' -> A
      'b' -> B
      'c' -> C
      'd' -> D



interpret (CpyVal x r)   (i,s) = (i+1, Seq.update (fromEnum r) x s)
interpret (CpyReg r1 r2) (i,s) = (i+1, Seq.update (fromEnum r2) (Seq.index s (fromEnum r1)) s)
interpret (Inc    r    ) (i,s) = (i+1, Seq.adjust (\x -> x+1) (fromEnum r) s)
interpret (Dec    r    ) (i,s) = (i+1, Seq.adjust (\x -> x-1) (fromEnum r) s)
interpret (JnzReg r  j ) (i,s) = if Seq.index s (fromEnum r) /= 0 then (i+j,s) else (i+1,s)
interpret (JnzVal x  j ) (i,s) = if x /= 0 then (i+j,s) else (i+1,s)

runProgram is = go
    where go (i,s) | i < V.length is = go $ interpret (is V.! i) (i,s)
                   | otherwise       = (i,s)

main = do

    let fn = "input-12-1.txt"
    (Right is) <- parse instructionsP fn <$> readFile fn

    let (_,regs1) = runProgram is (0,Seq.fromList [0,0,0,0])
        (_,regs2) = runProgram is (0,Seq.fromList [0,0,1,0])

    let result1 = Seq.index regs1 0
        result2 = Seq.index regs2 0

    putStrLn $ "result part one: " ++ show result1
    putStrLn $ "result part two: " ++ show result2

