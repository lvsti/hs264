-- Hs264.Parsing.SyntaxElement

module Hs264.Parsing.SyntaxElement where

import Data.Bits
import qualified Data.Bitstream.Lazy as BTL

-- big endian bitstream
type BitstreamBE = BTL.Bitstream BTL.Right


kMaxSynelBits :: Int
kMaxSynelBits = bitSize (undefined :: Int)

bitsToInt :: Int -> BitstreamBE -> Int
bitsToInt n bt = (BTL.toBits :: BitstreamBE -> Int) $ BTL.take n bt

extendSign :: (Integral a) => Int -> Int -> a
extendSign n nBitValue = if isNegative then fromIntegral (-baseValue) else fromIntegral baseValue
	where
		isNegative = testBit nBitValue (n-1)
		baseValue = clearBit nBitValue (n-1)


type BitParseError = String

data BitParseState =
    BitParseState {
        bpsBits :: BitstreamBE,
        bpsOffset :: Int
    }

instance Show BitParseState where
    show bps = "<ofs:" ++ show (bpsOffset bps) ++ ">"


newtype BitParse a =
    BitParse {
        runBitParse :: BitParseState -> Either BitParseError (a, BitParseState)
    }


getBitState :: BitParse BitParseState
getBitState = BitParse $ \bps -> Right (bps, bps)

putBitState :: BitParseState -> BitParse ()
putBitState bps = BitParse $ \_ -> Right ((), bps)

instance Monad BitParse where
    return x = BitParse $ \bps -> Right (x, bps)
    fail err = BitParse $ \bps -> Left $ err ++ " " ++ show bps
    parse >>= continuation = BitParse bareParse
        where
            bareParse bps =
                case runBitParse parse bps of
                    Left err -> Left err
                    Right (x, bps') -> runBitParse (continuation x) bps'


type SynelParse = BitParse Int


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
parseAEv = fail "AE(v) not implemented"

parseB8 :: SynelParse
parseB8 = parseFn 8

parseCEv :: SynelParse
parseCEv = fail "CE(v): not implemented"

-- spec 7.2
parseFn :: Int -> SynelParse
parseFn n
    | n > kMaxSynelBits = fail $ "F(n): integer overflow (" ++ show n ++ ">" ++ show kMaxSynelBits ++ ")"
    | otherwise =
        getBitState >>= \bps ->
        (let
            bt = bpsBits bps
            remaining = BTL.length bt
            value = bitsToInt n bt
            bps' = bps {
                bpsBits = BTL.drop n bt,
                bpsOffset = bpsOffset bps + n
            }
        in
            if remaining >= n then
                putBitState bps' >>
                return value
            else
                fail $ "F(n): unexpected end-of-stream (expected:" ++ show n ++ ", remaining:" ++ show remaining ++ ")"
        )

-- spec 7.2
parseIn :: Int -> SynelParse
parseIn n =
	parseFn n >>= \value ->
	return $ extendSign n value

-- spec 9.1.2
parseMEv :: SynelParse
parseMEv = fail "ME(v): use UE(v) and refer to 9.1.2 for the mapping"

-- spec 9.1.1
parseSEv :: SynelParse
parseSEv =
	parseUEv >>= \value ->
	(let
		absValue = (value + 1) `shiftR` 1
		mappedValue = if odd value then absValue else (-absValue)
	in
		return mappedValue
    )

-- spec 9.1.1
parseTEv :: Int -> SynelParse
parseTEv range
	| range > 1 = parseUEv
    | range < 1 = fail $ "TE(v): invalid range (" ++ show range ++ ")"
	| otherwise =
        parseFn 1 >>= \value ->
        return (1-value)

-- spec 7.2
parseUn :: Int -> SynelParse
parseUn = parseFn

-- spec 9.1
parseUEv :: SynelParse
parseUEv = 
    getBitState >>= \bps ->
    (let
        bt = bpsBits bps
        (prefix, suffix) = BTL.span (==False) bt
        leadingZeroBits = BTL.length prefix
        mantissa = bitsToInt leadingZeroBits $ BTL.tail suffix
        value = (1 `shiftL` leadingZeroBits) - 1 + mantissa
        numBits = (leadingZeroBits `shiftL` 1) + 1
        bps' = bps {
            bpsBits = BTL.drop numBits bt,
            bpsOffset = bpsOffset bps + numBits
        }
    in
        if BTL.length suffix > leadingZeroBits then
            putBitState bps' >>
            return value
        else
            fail "UE(v): incomplete exp-Golomb codeword"
    )


------------------------------------------------------------------------------
-- Syntax element data type
------------------------------------------------------------------------------

data Synel = Synel { synelName :: String,
					 synelType :: SynelType,
                     synelCategories :: [Int],
					 synelValidator :: Int -> Bool }

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
    Synel {
        synelName = sn,
        synelType = st,
        synelCategories = cat,
        synelValidator = const True
    }

mkSynelV :: String -> SynelType -> [Int] -> (Int -> Bool) -> Synel
mkSynelV sn st cat sv = baseSynel { synelValidator = sv }
	where
		baseSynel = mkSynel sn st cat


parseSynel :: Synel -> SynelParse
parseSynel syn =
	synelFunction (synelType syn) >>= \value ->
	if (synelValidator syn) value then
		return value
	else
		fail $ "validation of synel '" ++ show syn ++ "' failed, value = " ++ show value

