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
				 SynelTypeTEv |
				 SynelTypeUn Int |
				 SynelTypeUEv deriving (Eq, Show)
				 


parseSynelAEv :: BitstreamBE -> Maybe (BitstreamBE, Int)
parseSynelAEv = error "not implemented"

parseSynelB8 :: BitstreamBE -> Maybe (BitstreamBE, Int)
parseSynelB8 = parseSynelFn 8

parseSynelCEv :: BitstreamBE -> Maybe (BitstreamBE, Int)
parseSynelCEv = error "not implemented"

parseSynelFn :: Int -> BitstreamBE -> Maybe (BitstreamBE, Int)
parseSynelFn n bt
	| BTL.length bt < n || n > 32 = Nothing
	| otherwise = Just (BTL.drop n bt, bitsToInt n bt)

parseSynelIn :: Int -> BitstreamBE -> Maybe (BitstreamBE, Int)
parseSynelIn n bt
	| BTL.length bt < n || n > 32 = Nothing
	| otherwise = Just (BTL.drop n bt, extendSign n $ bitsToInt n bt)

parseSynelMEv :: BitstreamBE -> Maybe (BitstreamBE, Int)
parseSynelMEv = error "not implemented"

parseSynelSEv :: BitstreamBE -> Maybe (BitstreamBE, Int)
parseSynelSEv = error "not implemented"

parseSynelTEv :: BitstreamBE -> Maybe (BitstreamBE, Int)
parseSynelTEv = error "not implemented"

parseSynelUn :: Int -> BitstreamBE -> Maybe (BitstreamBE, Int)
parseSynelUn = parseSynelFn

parseSynelUEv :: BitstreamBE -> Maybe (BitstreamBE, Int)
parseSynelUEv = error "not implemented"

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
synelFunction SynelTypeTEv = parseSynelTEv
synelFunction (SynelTypeUn n) = parseSynelUn n
synelFunction SynelTypeUEv = parseSynelUEv

