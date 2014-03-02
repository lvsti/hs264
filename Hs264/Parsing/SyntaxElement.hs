-- Hs264.Parsing.SyntaxElement

module Hs264.Parsing.SyntaxElement where

import qualified Data.Bitstream.Lazy as BTL
import qualified Data.Map.Strict as M
import Data.Bits
import Debug.Trace
import qualified Hs264.Data.SparseArray as SA

-- big endian bitstream
type BitstreamBE = BTL.Bitstream BTL.Right


bitsToInt :: Int -> BitstreamBE -> Int
bitsToInt n bt = (BTL.toBits :: BitstreamBE -> Int) $ BTL.take n bt

extendSign :: (Integral a) => Int -> Int -> a
extendSign n nBitValue = if isNegative then fromIntegral (-baseValue) else fromIntegral baseValue
	where
		isNegative = testBit nBitValue (n-1)
		baseValue = clearBit nBitValue (n-1)


------------------------------------------------------------------------------
-- Syntax element types (spec 7.2)
------------------------------------------------------------------------------
data SynelType = SynelTypeAEv |
				 SynelTypeB8 |
				 SynelTypeCEv |
				 SynelTypeFn Int |
				 SynelTypeIn Int |
				 SynelTypeMEv |
				 SynelTypeSEv |
				 SynelTypeTEv Int |
				 SynelTypeUn Int |
				 SynelTypeUEv deriving (Eq)

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


synelFunction :: SynelType -> BitstreamBE -> Maybe (BitstreamBE, Int)
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


parseAEv :: BitstreamBE -> Maybe (BitstreamBE, Int)
parseAEv = error "not implemented"

parseB8 :: BitstreamBE -> Maybe (BitstreamBE, Int)
parseB8 = parseFn 8

parseCEv :: BitstreamBE -> Maybe (BitstreamBE, Int)
parseCEv = error "not implemented"

-- spec 7.2
parseFn :: Int -> BitstreamBE -> Maybe (BitstreamBE, Int)
parseFn n bt
	| BTL.length bt < n || n > 32 = Nothing
	| otherwise = Just (BTL.drop n bt, bitsToInt n bt)

-- spec 7.2
parseIn :: Int -> BitstreamBE -> Maybe (BitstreamBE, Int)
parseIn n bt =
	parseFn n bt >>= \(bt', value) ->
	Just (bt', extendSign n value)

-- spec 9.1.2
parseMEv :: BitstreamBE -> Maybe (BitstreamBE, Int)
parseMEv = error "use UE(v) and refer to 9.1.2 for the mapping"

-- spec 9.1.1
parseSEv :: BitstreamBE -> Maybe (BitstreamBE, Int)
parseSEv bt =
	parseUEv bt >>= \(bt', value) ->
	let
		absValue = (value + 1) `shiftR` 1
		mappedValue = if odd value then absValue else (-absValue)
	in
		Just (bt', mappedValue)

-- spec 9.1.1
parseTEv :: Int -> BitstreamBE -> Maybe (BitstreamBE, Int)
parseTEv range bt
	| range > 1 = parseUEv bt
	| range < 1 || BTL.null bt = Nothing
	| otherwise = Just (BTL.tail bt, if BTL.head bt then 0 else 1)

-- spec 7.2
parseUn :: Int -> BitstreamBE -> Maybe (BitstreamBE, Int)
parseUn = parseFn

-- spec 9.1
parseUEv :: BitstreamBE -> Maybe (BitstreamBE, Int)
parseUEv bt
	| BTL.length suffix < leadingZeroBits + 1 = Nothing
	| otherwise = Just (BTL.drop (2 * leadingZeroBits + 1) bt, value)
	where
		(prefix, suffix) = BTL.span (==False) bt
		leadingZeroBits = BTL.length prefix
		mantissa = bitsToInt leadingZeroBits $ BTL.tail suffix
		value = (1 `shiftL` leadingZeroBits) - 1 + mantissa



------------------------------------------------------------------------------
-- Syntax element data type
------------------------------------------------------------------------------

data Synel = Synel { synelName :: String,
					 synelType :: SynelType,
					 synelValidator :: Int -> Bool }

instance Eq Synel where
	s1 == s2 = synelName s1 == synelName s2
	
instance Ord Synel where
    compare s1 s2 = compare (synelName s1) (synelName s2)
    (<) s1 s2 = (<) (synelName s1) (synelName s2)
    (>) s1 s2 = (>) (synelName s1) (synelName s2)

instance Show Synel where
	show syn = show (synelName syn) ++ " :: " ++ show (synelType syn)

					 
mkSynel :: String -> SynelType -> Synel
mkSynel sn st = Synel { synelName = sn, synelType = st, synelValidator = const True }

mkSynelV :: String -> SynelType -> (Int -> Bool) -> Synel
mkSynelV sn st sv = baseSynel { synelValidator = sv }
	where
		baseSynel = mkSynel sn st


------------------------------------------------------------------------------
-- Syntax element dictionary
------------------------------------------------------------------------------

data SynelValue = SVScalar Int
				| SVArray [Int]
				| SVSparseArray (SA.SparseArray Int)
				deriving (Eq,Show)

type SynelDictionary = M.Map Synel SynelValue

emptySd :: SynelDictionary
emptySd = M.empty

sdHasKey :: SynelDictionary -> Synel -> Bool
sdHasKey sd key = M.member key sd

sdHasKeys :: SynelDictionary -> [Synel] -> Bool
sdHasKeys sd ks = all (sdHasKey sd) ks


sdScalar :: SynelDictionary -> Synel -> Int
sdScalar sd key = scalarValue
	where
		(SVScalar scalarValue) = sd M.! key

sdArray :: SynelDictionary -> Synel -> [Int]
sdArray sd key = arrayValue
	where
		(SVArray arrayValue) = sd M.! key
		
sdSparseArray :: SynelDictionary -> Synel -> SA.SparseArray Int
sdSparseArray sd key = saValue
	where
		(SVSparseArray saValue) = sd M.! key


sdSetScalar :: SynelDictionary -> Synel -> Int -> SynelDictionary
sdSetScalar sd key value = M.insert key (SVScalar value) sd

sdSetArray :: SynelDictionary -> Synel -> [Int] -> SynelDictionary
sdSetArray sd key vs = M.insert key (SVArray vs) sd

sdAppendToArray :: SynelDictionary -> Synel -> [Int] -> SynelDictionary
sdAppendToArray sd key vs = M.insertWith updateArray key (SVArray vs) sd
	where
		updateArray :: SynelValue -> SynelValue -> SynelValue
		updateArray (SVArray newvs) (SVArray oldvs) = SVArray (oldvs ++ newvs)

sdSetSparseArray :: SynelDictionary -> Synel -> SA.SparseArray Int -> SynelDictionary
sdSetSparseArray sd key sa = M.insert key (SVSparseArray sa) sd

sdAddToSparseArray :: SynelDictionary -> Synel -> SA.SparseIndex -> Int -> SynelDictionary
sdAddToSparseArray sd key ix v = M.insertWith updateSparseArray key (SVSparseArray $ SA.singleton ix v) sd
	where
		updateSparseArray :: SynelValue -> SynelValue -> SynelValue
		updateSparseArray (SVSparseArray newsa) (SVSparseArray oldsa) = SVSparseArray $ SA.union newsa oldsa


------------------------------------------------------------------------------
-- Syntax element parsing
------------------------------------------------------------------------------

type SynelParseState = (BitstreamBE, SynelDictionary)

parse :: Synel -> SynelParseState -> Maybe SynelParseState
parse syn (bt, sd) =
	parseSynel bt syn >>= \(bt', value) ->
	(let
		sd' = sdSetScalar sd syn value
	in
		trace (show syn ++ " = " ++ show value ++ " (rem:" ++ show (BTL.length bt') ++ ")") $ return (bt', sd')
	)

	
parseA :: Synel -> SynelParseState -> Maybe SynelParseState
parseA syn (bt, sd) =
	parseSynel bt syn >>= \(bt', value) ->
	(let
		sd' = sdAppendToArray sd syn [value]
	in
		trace (show syn ++ " = " ++ show value ++ " (rem:" ++ show (BTL.length bt') ++ ")") $ return (bt', sd')
	)


parseSA :: Synel -> SA.SparseIndex -> SynelParseState -> Maybe SynelParseState
parseSA syn ix (bt, sd) =
	parseSynel bt syn >>= \(bt', value) ->
	(let
		sd' = sdAddToSparseArray sd syn ix value
	in
		trace (show syn ++ " = " ++ show value ++ " (rem:" ++ show (BTL.length bt') ++ ")") $ return (bt', sd')
	)


parseSynel :: BitstreamBE -> Synel -> Maybe (BitstreamBE, Int)
parseSynel bt syn =
	synelFunction (synelType syn) bt >>= \(bt', value) ->
	if (synelValidator syn) value then
		return (bt', value)
	else
		trace ("validation of synel [" ++ show syn ++ "] failed, value = " ++ show value) Nothing
		

parseForEach :: [Int]
			 -> (Int -> SynelParseState -> Maybe SynelParseState)
			 -> SynelParseState
			 -> Maybe SynelParseState
parseForEach [] _ state = return state
parseForEach vs f state =
	return state >>=
	f (head vs) >>=
	parseForEach (tail vs) f


parseWhile :: (SynelParseState -> Bool)
		   -> (SynelParseState -> Maybe SynelParseState)
		   -> SynelParseState
		   -> Maybe SynelParseState
parseWhile pr f state@(bt, sd)
	| pr state = return state >>= f >>= parseWhile pr f
	| otherwise = return state

