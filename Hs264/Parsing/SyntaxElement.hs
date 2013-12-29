-- Hs264.Parsing.SyntaxElement

module Hs264.Parsing.SyntaxElement where

import qualified Data.Bitstream.Lazy as BTL
import Data.Bits

-- big endian bitstream
type BitstreamBE = BTL.Bitstream BTL.Right


bitsToInt :: Int -> BitstreamBE -> Int
bitsToInt n bt = (BTL.toBits :: BitstreamBE -> Int) $ BTL.take n bt

extendSign :: (Integral a) => Int -> Int -> a
extendSign n nBitValue = if isNegative then fromIntegral (-baseValue) else fromIntegral baseValue
	where
		isNegative = testBit nBitValue (n-1)
		baseValue = clearBit nBitValue (n-1)


data SynelType = SynelTypeAEv |
				 SynelTypeB8 |
				 SynelTypeCEv |
				 SynelTypeFn Int |
				 SynelTypeIn Int |
				 SynelTypeMEv |
				 SynelTypeSEv |
				 SynelTypeTEv Int |
				 SynelTypeUn Int |
				 SynelTypeUEv deriving (Eq, Show)
				 


parseSynelAEv :: BitstreamBE -> Maybe (BitstreamBE, Int)
parseSynelAEv = error "not implemented"

parseSynelB8 :: BitstreamBE -> Maybe (BitstreamBE, Int)
parseSynelB8 = parseSynelFn 8

parseSynelCEv :: BitstreamBE -> Maybe (BitstreamBE, Int)
parseSynelCEv = error "not implemented"

-- spec 7.2
parseSynelFn :: Int -> BitstreamBE -> Maybe (BitstreamBE, Int)
parseSynelFn n bt
	| BTL.length bt < n || n > 32 = Nothing
	| otherwise = Just (BTL.drop n bt, bitsToInt n bt)

-- spec 7.2
parseSynelIn :: Int -> BitstreamBE -> Maybe (BitstreamBE, Int)
parseSynelIn n bt =
	parseSynelFn n bt >>= \(bt', value) ->
	return (bt', extendSign n value)

-- spec 9.1.2
parseSynelMEv :: BitstreamBE -> Maybe (BitstreamBE, Int)
parseSynelMEv = error "use UE(v) and refer to 9.1.2 for the mapping"

-- spec 9.1.1
parseSynelSEv :: BitstreamBE -> Maybe (BitstreamBE, Int)
parseSynelSEv bt =
	parseSynelUEv bt >>= \(bt', value) ->
	let
		absValue = (value + 1) `shiftR` 1
		mappedValue = if odd value then absValue else (-absValue)
	in
		return (bt', mappedValue)

-- spec 9.1.1
parseSynelTEv :: Int -> BitstreamBE -> Maybe (BitstreamBE, Int)
parseSynelTEv range bt
	| range > 1 = parseSynelUEv bt
	| range < 1 || BTL.null bt = Nothing
	| otherwise = Just (BTL.tail bt, if BTL.head bt then 0 else 1)

-- spec 7.2
parseSynelUn :: Int -> BitstreamBE -> Maybe (BitstreamBE, Int)
parseSynelUn = parseSynelFn

-- spec 9.1
parseSynelUEv :: BitstreamBE -> Maybe (BitstreamBE, Int)
parseSynelUEv bt
	| BTL.length suffix < leadingZeroBits + 1 = Nothing
	| otherwise = Just (BTL.drop (2 * leadingZeroBits + 1) bt, value)
	where
		(prefix, suffix) = BTL.span (==False) bt
		leadingZeroBits = BTL.length prefix
		mantissa = bitsToInt leadingZeroBits $ BTL.tail suffix
		value = (1 `shiftL` leadingZeroBits) - 1 + mantissa


parseSynel :: SynelType -> (BitstreamBE, [Int]) -> Maybe (BitstreamBE, [Int])
parseSynel syn = parseAndValidateSynel syn (\_ -> True)

parseAndValidateSynel :: SynelType -> (Int -> Bool) -> (BitstreamBE, [Int]) -> Maybe (BitstreamBE, [Int])
parseAndValidateSynel syn vf (bt, vs) = 
	synelFunction syn bt >>= \(bt', value) ->
	if vf value then
		return (bt', vs ++ [value])
	else
		Nothing


synelFunction :: SynelType -> BitstreamBE -> Maybe (BitstreamBE, Int)
synelFunction SynelTypeAEv = parseSynelAEv
synelFunction SynelTypeB8 = parseSynelB8
synelFunction SynelTypeCEv = parseSynelCEv
synelFunction (SynelTypeFn n) = parseSynelFn n
synelFunction (SynelTypeIn n) = parseSynelIn n
synelFunction SynelTypeMEv = parseSynelMEv
synelFunction SynelTypeSEv = parseSynelSEv
synelFunction (SynelTypeTEv r) = parseSynelTEv r
synelFunction (SynelTypeUn n) = parseSynelUn n
synelFunction SynelTypeUEv = parseSynelUEv

