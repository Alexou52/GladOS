{-- 
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- BytecodeFile.hs
--}

module BytecodeFile (compileToBinaryFile, loadFromBinaryFile, CompiledFile(..), fileExtension, magicNumber) where

import Bytecode
import Data.Word (Word8)
import Data.Bits (xor)
import Data.Char (ord, chr)
import qualified Data.ByteString as BS
import Data.List (intercalate)
import Text.Read (readMaybe)

fileExtension :: String
fileExtension = ".glc"

magicNumber :: [Word8]
magicNumber = [0x47, 0x4C, 0x44, 0x43]

formatVersion :: Word8
formatVersion = 1

encryptionKey :: [Word8]
encryptionKey = [0xDE, 0xAD, 0xBE, 0xEF, 0xCA, 0xFE, 0xBA, 0xBE, 0x13, 0x37, 0x42, 0x69, 0xAB, 0xCD, 0xEF, 0x01]

data CompiledFile = CompiledFile
    { cfMagic :: [Word8], cfVersion :: Word8, cfChecksum :: Int, cfBytecode :: Program }
    deriving (Show)

serializeValue :: Value -> String
serializeValue VNil = "N"
serializeValue (VInt n) = "I" ++ show n
serializeValue (VBool True) = "T"
serializeValue (VBool False) = "F"
serializeValue (VString s) = "S" ++ show (length s) ++ ":" ++ s
serializeValue (VList vs) = "L" ++ show (length vs) ++ ":" ++ intercalate "," (map serializeValue vs)
serializeValue (VClosure params body) = 
    "C" ++ show (length params) ++ ":" ++ intercalate "," params ++ "|" ++ 
    show (length body) ++ ":" ++ serializeProgramInline body
serializeValue (VBuiltin name) = "B" ++ show (length name) ++ ":" ++ name

serializeInstr :: Instruction -> String
serializeInstr (PUSH v) = "PUSH " ++ serializeValue v
serializeInstr POP = "POP"
serializeInstr DUP = "DUP"
serializeInstr SWAP = "SWAP"
serializeInstr (LOAD n) = "LOAD " ++ n
serializeInstr (STORE n) = "STORE " ++ n
serializeInstr ADD = "ADD"
serializeInstr SUB = "SUB"
serializeInstr MUL = "MUL"
serializeInstr DIV = "DIV"
serializeInstr MOD = "MOD"
serializeInstr POW = "POW"
serializeInstr FACT = "FACT"
serializeInstr EQ_ = "EQ"
serializeInstr NEQ = "NEQ"
serializeInstr LT_ = "LT"
serializeInstr GT_ = "GT"
serializeInstr LTE = "LTE"
serializeInstr GTE = "GTE"
serializeInstr AND = "AND"
serializeInstr OR = "OR"
serializeInstr NOT = "NOT"
serializeInstr NEG = "NEG"
serializeInstr CONCAT = "CONCAT"
serializeInstr PRINT = "PRINT"
serializeInstr (MAKE_LIST n) = "MKLIST " ++ show n
serializeInstr LIST_GET = "LGET"
serializeInstr LIST_LEN = "LLEN"
serializeInstr (CALL n) = "CALL " ++ show n
serializeInstr RET = "RET"
serializeInstr (JMP a) = "JMP " ++ show a
serializeInstr (JMP_IF_FALSE a) = "JIF " ++ show a
serializeInstr (JMP_IF_TRUE a) = "JIT " ++ show a
serializeInstr BREAK = "BRK"
serializeInstr CONTINUE = "CNT"
serializeInstr (MAKE_CLOSURE params body) = 
    "MKCLS " ++ show (length params) ++ ":" ++ intercalate "," params ++ "|" ++ 
    show (length body) ++ ":" ++ serializeProgramInline body
serializeInstr NOP = "NOP"
serializeInstr HALT = "HALT"
serializeInstr (LINE n) = "LINE " ++ show n

serializeProgram :: Program -> String
serializeProgram = intercalate "\n" . map serializeInstr

serializeProgramInline :: Program -> String
serializeProgramInline = intercalate ";" . map serializeInstr

deserializeValue :: String -> Maybe (Value, String)
deserializeValue ('N':r) = Just (VNil, r)
deserializeValue ('T':r) = Just (VBool True, r)
deserializeValue ('F':r) = Just (VBool False, r)
deserializeValue ('I':r) = let (n, rest) = span (\c -> c == '-' || c `elem` ['0'..'9']) r
    in fmap (\v -> (VInt v, rest)) (readMaybe n)
deserializeValue ('S':r) = let (len, after) = span (`elem` ['0'..'9']) r
    in case (readMaybe len, after) of
        (Just l, ':':c) -> Just (VString $ take l c, drop l c)
        _ -> Nothing
deserializeValue ('L':r) = let (len, after) = span (`elem` ['0'..'9']) r
    in case (readMaybe len :: Maybe Int, after) of
        (Just 0, ':':rest) -> Just (VList [], rest)
        (Just l, ':':rest) -> deserializeListValues l rest >>= \(vs, remaining) -> Just (VList vs, remaining)
        _ -> Nothing
deserializeValue ('B':r) = let (len, after) = span (`elem` ['0'..'9']) r
    in case (readMaybe len, after) of
        (Just l, ':':c) -> Just (VBuiltin $ take l c, drop l c)
        _ -> Nothing
deserializeValue ('C':r) = 
    let (len, after) = span (`elem` ['0'..'9']) r
    in case (readMaybe len :: Maybe Int, after) of
        (Just pc, ':':rest) ->
            let (paramsRest, r1) = break (== '|') rest
                params = if pc == 0 then [] else splitOn ',' paramsRest
                afterParams = drop 1 r1
                (bodyLen, afterLen) = span (`elem` ['0'..'9']) afterParams
            in case (readMaybe bodyLen :: Maybe Int, afterLen) of
                (Just _, ':':body) -> (,) . VClosure params <$> deserializeProgramInline body <*> Just ""
                _ -> Nothing
        _ -> Nothing
deserializeValue _ = Nothing

deserializeListValues :: Int -> String -> Maybe ([Value], String)
deserializeListValues 0 s = Just ([], s)
deserializeListValues n s = do
    (v, rest) <- deserializeValue s
    let remaining = if null rest then rest else if head rest == ',' then tail rest else rest
    (vs, final) <- deserializeListValues (n - 1) remaining
    Just (v:vs, final)

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn d s = let (w, r) = break (== d) s in w : case r of { [] -> []; (_:t) -> splitOn d t }

deserializeInstr :: String -> Maybe Instruction
deserializeInstr "POP" = Just POP
deserializeInstr "DUP" = Just DUP
deserializeInstr "SWAP" = Just SWAP
deserializeInstr "ADD" = Just ADD
deserializeInstr "SUB" = Just SUB
deserializeInstr "MUL" = Just MUL
deserializeInstr "DIV" = Just DIV
deserializeInstr "MOD" = Just MOD
deserializeInstr "POW" = Just POW
deserializeInstr "FACT" = Just FACT
deserializeInstr "EQ" = Just EQ_
deserializeInstr "NEQ" = Just NEQ
deserializeInstr "LT" = Just LT_
deserializeInstr "GT" = Just GT_
deserializeInstr "LTE" = Just LTE
deserializeInstr "GTE" = Just GTE
deserializeInstr "AND" = Just AND
deserializeInstr "OR" = Just OR
deserializeInstr "NOT" = Just NOT
deserializeInstr "NEG" = Just NEG
deserializeInstr "CONCAT" = Just CONCAT
deserializeInstr "PRINT" = Just PRINT
deserializeInstr "LGET" = Just LIST_GET
deserializeInstr "LLEN" = Just LIST_LEN
deserializeInstr "BRK" = Just BREAK
deserializeInstr "CNT" = Just CONTINUE
deserializeInstr "RET" = Just RET
deserializeInstr "NOP" = Just NOP
deserializeInstr "HALT" = Just HALT
deserializeInstr s
    | take 5 s == "PUSH " = fst <$> deserializeValue (drop 5 s) >>= Just . PUSH
    | take 5 s == "LOAD " = Just $ LOAD (drop 5 s)
    | take 6 s == "STORE " = Just $ STORE (drop 6 s)
    | take 5 s == "CALL " = CALL <$> readMaybe (drop 5 s)
    | take 4 s == "JMP " = JMP <$> readMaybe (drop 4 s)
    | take 4 s == "JIF " = JMP_IF_FALSE <$> readMaybe (drop 4 s)
    | take 4 s == "JIT " = JMP_IF_TRUE <$> readMaybe (drop 4 s)
    | take 7 s == "MKLIST " = MAKE_LIST <$> readMaybe (drop 7 s)
    | take 6 s == "MKCLS " = deserializeMakeClosure (drop 6 s)
    | take 5 s == "LINE " = LINE <$> readMaybe (drop 5 s)
    | otherwise = Nothing

deserializeMakeClosure :: String -> Maybe Instruction
deserializeMakeClosure s =
    let (len, after) = span (`elem` ['0'..'9']) s
    in case (readMaybe len :: Maybe Int, after) of
        (Just pc, ':':rest) ->
            let (paramsRest, r1) = break (== '|') rest
                params = if pc == 0 then [] else splitOn ',' paramsRest
                afterParams = drop 1 r1
                (bodyLen, afterLen) = span (`elem` ['0'..'9']) afterParams
            in case (readMaybe bodyLen :: Maybe Int, afterLen) of
                (Just _, ':':body) -> MAKE_CLOSURE params <$> deserializeProgramInline body
                _ -> Nothing
        _ -> Nothing

deserializeProgram :: String -> Maybe Program
deserializeProgram = sequence . map deserializeInstr . filter (not . null) . lines

deserializeProgramInline :: String -> Maybe Program
deserializeProgramInline = sequence . map deserializeInstr . filter (not . null) . splitOn ';'

encrypt :: [Word8] -> [Word8] -> [Word8]
encrypt key = zipWith xor (cycle key)

stringToBytes :: String -> [Word8]
stringToBytes = map (fromIntegral . ord)

bytesToString :: [Word8] -> String
bytesToString = map (chr . fromIntegral)

checksum :: [Word8] -> Int
checksum bs = (sum (map fromIntegral bs) * 31337 + foldr xor 0 (map fromIntegral bs)) `mod` (2^(31::Int) - 1)

compileToBinaryFile :: FilePath -> Program -> IO ()
compileToBinaryFile path prog = do
    let plain = stringToBytes $ serializeProgram prog
        cs = checksum plain
        enc = encrypt encryptionKey plain
        content = magicNumber ++ [formatVersion] ++ word32ToBytes cs ++ enc
    BS.writeFile path (BS.pack content)
    putStrLn $ "Compiled to: " ++ path
    putStrLn $ "  Size: " ++ show (length content) ++ " bytes"
    putStrLn $ "  Checksum: 0x" ++ toHex cs

loadFromBinaryFile :: FilePath -> IO (Either String Program)
loadFromBinaryFile path = parseFile . BS.unpack <$> BS.readFile path

parseFile :: [Word8] -> Either String Program
parseFile c
    | length c < 9 = Left "File too small"
    | take 4 c /= magicNumber = Left "Invalid file format"
    | c !! 4 /= formatVersion = Left $ "Unsupported version: " ++ show (c !! 4)
    | otherwise =
        let stored = bytesToWord32 $ take 4 $ drop 5 c
            dec = encrypt encryptionKey $ drop 9 c
            actual = checksum dec
        in if stored /= actual
           then Left $ "Checksum mismatch: expected 0x" ++ toHex stored ++ ", got 0x" ++ toHex actual
           else maybe (Left "Failed to deserialize") Right $ deserializeProgram $ bytesToString dec

word32ToBytes :: Int -> [Word8]
word32ToBytes n = [f 3, f 2, f 1, f 0]
  where f :: Int -> Word8
        f i = fromIntegral $ (n `div` (256 ^ i)) `mod` 256

bytesToWord32 :: [Word8] -> Int
bytesToWord32 [a,b,c,d] = sum $ zipWith (*) (map fromIntegral [a,b,c,d]) [256^(3::Int), 256^(2::Int), 256, 1]
bytesToWord32 _ = 0

toHex :: Int -> String
toHex 0 = "0"
toHex n = reverse $ go n
  where
    go 0 = ""
    go x = hexDigit (x `mod` 16) : go (x `div` 16)
    hexDigit d = if d < 10 then chr (ord '0' + d) else chr (ord 'A' + d - 10)
