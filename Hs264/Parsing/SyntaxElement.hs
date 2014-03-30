-- Hs264.Parsing.SyntaxElement

module Hs264.Parsing.SyntaxElement (
    Synel,
    SynelType(..),
    mkSynel,
    mkSynelV,
    parseSynel
    ) where

import Data.Bits
import qualified Data.Bitstream.Lazy as BTL
import Hs264.Parsing.BitParse


------------------------------------------------------------------------------
-- Syntax element types (spec 7.2)
------------------------------------------------------------------------------
data SynelType = SynelTypeAEv
               | SynelTypeB8
               | SynelTypeCEv
               | SynelTypeFn Int
               | SynelTypeIn Int
               | SynelTypeMEv
               | SynelTypeSEv
               | SynelTypeTEv Int
               | SynelTypeUn Int
               | SynelTypeUEv
               deriving (Eq)

instance Show SynelType where
    show SynelTypeAEv = "ae(v)"
    show SynelTypeB8 = "b(8)"
    show SynelTypeCEv = "ce(v)"
    show (SynelTypeFn n) = "f(" ++ show n ++ ")"
    show (SynelTypeIn n) = "i(" ++ show n ++ ")"
    show SynelTypeMEv = "me(v)"
    show SynelTypeSEv = "se(v)"
    show (SynelTypeTEv r) = "te(v|" ++ show r ++ ")"
    show (SynelTypeUn n) = "u(" ++ show n ++ ")"
    show SynelTypeUEv = "ue(v)"


------------------------------------------------------------------------------
-- Syntax element data type
------------------------------------------------------------------------------

data Synel =
    SynelCTOR {
        synelName :: String,
        synelType :: SynelType,
        synelCategories :: [Int],
        synelValidator :: Int -> Bool
    }

instance Eq Synel where
    s1 == s2 = synelName s1 == synelName s2

instance Ord Synel where
    compare s1 s2 = compare (synelName s1) (synelName s2)
    (<) s1 s2 = (<) (synelName s1) (synelName s2)
    (>) s1 s2 = (>) (synelName s1) (synelName s2)

instance Show Synel where
    show syn = show (synelName syn) ++ " C" ++ show (synelCategories syn) ++ " :: " ++ show (synelType syn)


mkSynel :: String -> SynelType -> [Int] -> Synel
mkSynel sn st cat =
    SynelCTOR {
        synelName = sn,
        synelType = st,
        synelCategories = cat,
        synelValidator = const True
    }


mkSynelV :: String -> SynelType -> [Int] -> (Int -> Bool) -> Synel
mkSynelV sn st cat sv = baseSynel { synelValidator = sv }
    where
        baseSynel = mkSynel sn st cat



------------------------------------------------------------------------------
-- Syntax element parsing
------------------------------------------------------------------------------

type SynelParse = BitParse Int


parseSynel :: Synel -> SynelParse
parseSynel syn = do
    value <- synelFunction (synelType syn)
    if (synelValidator syn) value then
        return value
    else
        failBP $ "validation of synel '" ++ show syn ++ "' failed, value = " ++ show value


synelFunction :: SynelType -> SynelParse
synelFunction SynelTypeAEv = parseAEv
synelFunction SynelTypeB8 = parseB8
synelFunction SynelTypeCEv = parseCEv
synelFunction (SynelTypeFn n) = parseFn n
synelFunction (SynelTypeIn n) = parseIn n
synelFunction SynelTypeMEv = parseMEv
synelFunction SynelTypeSEv = parseSEv
synelFunction (SynelTypeTEv r) = parseTEv r
synelFunction (SynelTypeUn n) = parseUn n
synelFunction SynelTypeUEv = parseUEv


parseAEv :: SynelParse
parseAEv = failBP "AE(v) not implemented"

parseB8 :: SynelParse
parseB8 = parseFn 8

parseCEv :: SynelParse
parseCEv = failBP "CE(v): not implemented"

-- spec 7.2
parseFn :: Int -> SynelParse
parseFn n
    | n > kMaxSynelBits = failBP $ "F(n): integer overflow (" ++ show n ++ ">" ++ show kMaxSynelBits ++ ")"
    | otherwise = do
        bps <- getBPState
        let bt = bpsBits bps
            remaining = BTL.length bt
            value = bitsToInt n bt
            bps' = bps {
                bpsBits = BTL.drop n bt,
                bpsOffset = bpsOffset bps + n
            }

        if remaining >= n then do
            putBPState bps'
            return value
        else
            failBP $ "F(n): unexpected end-of-stream (expected:" ++ show n ++ ", remaining:" ++ show remaining ++ ")"

-- spec 7.2
parseIn :: Int -> SynelParse
parseIn n = do
    value <- parseFn n
    return $ extendSign n value

-- spec 9.1.2
parseMEv :: SynelParse
parseMEv = failBP "ME(v): use UE(v) and refer to 9.1.2 for the mapping"

-- spec 9.1.1
parseSEv :: SynelParse
parseSEv = do
    value <- parseUEv
    let absValue = (value + 1) `shiftR` 1
    return $ if odd value then absValue else (-absValue)

-- spec 9.1.1
parseTEv :: Int -> SynelParse
parseTEv range
    | range > 1 = parseUEv
    | range < 1 = failBP $ "TE(v): invalid range (" ++ show range ++ ")"
    | otherwise = do
        value <- parseFn 1
        return (1-value)

-- spec 7.2
parseUn :: Int -> SynelParse
parseUn = parseFn

-- spec 9.1
parseUEv :: SynelParse
parseUEv = do
    bps <- getBPState
    let bt = bpsBits bps
        (prefix, suffix) = BTL.span (==False) bt
        leadingZeroBits = BTL.length prefix
        mantissa = bitsToInt leadingZeroBits $ BTL.tail suffix
        value = (1 `shiftL` leadingZeroBits) - 1 + mantissa
        numBits = (leadingZeroBits `shiftL` 1) + 1
        bps' = bps {
            bpsBits = BTL.drop numBits bt,
            bpsOffset = bpsOffset bps + numBits
        }

    if BTL.length suffix > leadingZeroBits then do
        putBPState bps'
        return value
    else
        failBP "UE(v): incomplete exp-Golomb codeword"



------------------------------------------------------------------------------
-- Helper functions
------------------------------------------------------------------------------

kMaxSynelBits :: Int
kMaxSynelBits = bitSize (undefined :: Int)

bitsToInt :: Int -> BitstreamBE -> Int
bitsToInt n bt = (BTL.toBits :: BitstreamBE -> Int) $ BTL.take n bt

extendSign :: (Integral a) => Int -> Int -> a
extendSign n nBitValue = if isNegative then fromIntegral (-baseValue) else fromIntegral baseValue
    where
        isNegative = testBit nBitValue (n-1)
        baseValue = clearBit nBitValue (n-1)

