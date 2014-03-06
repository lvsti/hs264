-- Hs264.Parsing.RBSP.Internal

module Hs264.Parsing.RBSP.Internal where

import Data.Bits
import qualified Data.Bitstream.Lazy as BTL
import Data.Maybe
import Debug.Trace

import Hs264.Parsing.SyntaxElement
import Hs264.Types


-------------------------------------------------------------------------------
-- Bitstream helper functions
-------------------------------------------------------------------------------

-- byte alignment compared to a reference bitstream
isByteAligned :: BitstreamBE -> BitstreamBE -> Bool
isByteAligned bt0 bt1 = (BTL.length bt0 - BTL.length bt1) `mod` 8 == 0


-- number of bits required to represent an integer value
bitWidth :: (Integral a, Bits a) => a -> Int
bitWidth 0 = 1
bitWidth 1 = 1
bitWidth n | n > 0 = 1 + bitWidth (n `shiftR` 1)



-------------------------------------------------------------------------------
-- RBSP helper functions
-------------------------------------------------------------------------------


-- spec 7.2
checkForMoreRbspData :: BitstreamBE -> Maybe (BitstreamBE, Bool)
checkForMoreRbspData bt
	| BTL.null bt || not (isJust maybe1Idx) || numDataBits < 0 = trace "ERROR: checkMoreRbspData: missing RBSP lead-out" Nothing
	| otherwise = return (bt, numDataBits > 0)
	where
		-- we have to find the last 1 bit from the rear (rbsp_stop_one_bit)
		maybe1Idx = BTL.findIndex id $ BTL.reverse bt
		numDataBits = BTL.length bt - (fromJust maybe1Idx + 1)


-- spec 7.3.2.11
parseRbspTrailingBits :: BitstreamBE -> BitstreamBE -> Maybe BitstreamBE
parseRbspTrailingBits bt0 bt =
	Just (bt, emptySd) >>=
	parse synelRbspStopOneBit >>=
	parseWhile (\(btp, sdp) -> not $ isByteAligned bt0 btp) (\(bt1, sd1) ->
		Just (bt1, sd1) >>=
		parse synelRbspAlignmentZeroBit
	) >>= \(bt2, sd2) ->
	return bt2
	
	where
		synelRbspStopOneBit = mkSynelV "rbsp_stop_one_bit" (SynelTypeUn 1) (==1)
		synelRbspAlignmentZeroBit = mkSynelV "rbsp_alignment_zero_bit" (SynelTypeUn 1) (==0)



-------------------------------------------------------------------------------
-- Scaling lists
-------------------------------------------------------------------------------

kScalingListFlat4x4 = replicate 16 (16 :: Arithmetic)
kScalingListFlat8x8 = replicate 64 (16 :: Arithmetic)

kInferredScalingListMatrix = (replicate 6 kScalingListFlat4x4) ++ (replicate 6 kScalingListFlat8x8)


-- spec Table 7-3
kScalingListDefault4x4Intra = [6,13,13,20,20,20,28,28, 28,28,32,32,32,37,37,42] :: [Arithmetic]
kScalingListDefault4x4Inter = [10,14,14,20,20,20,24,24, 24,24,27,27,27,30,30,34] :: [Arithmetic]

-- spec Table 7-4
kScalingListDefault8x8Intra = [6,10,10,13,11,13,16,16, 16,16,18,18,18,18,18,23,
							   23,23,23,23,23,25,25,25, 25,25,25,25,27,27,27,27,
							   27,27,27,27,29,29,29,29, 29,29,29,31,31,31,31,31,
							   31,33,33,33,33,33,36,36, 36,36,38,38,38,40,40,42] :: [Arithmetic]

kScalingListDefault8x8Inter = [9,13,13,15,13,15,17,17,
							   17,17,19,19,19,19,19,21,
							   21,21,21,21,21,22,22,22,
							   22,22,22,22,24,24,24,24,
							   24,24,24,24,25,25,25,25,
							   25,25,25,27,27,27,27,27,
							   27,28,28,28,28,28,30,30,
							   30,30,32,32,32,33,33,35] :: [Arithmetic]

-- spec Table 7-2
kDefaultScalingListMatrix = [kScalingListDefault4x4Intra,
							 kScalingListDefault4x4Intra,
							 kScalingListDefault4x4Intra,
							 kScalingListDefault4x4Inter,
							 kScalingListDefault4x4Inter,
							 kScalingListDefault4x4Inter,
							 kScalingListDefault8x8Intra,
							 kScalingListDefault8x8Inter,
							 kScalingListDefault8x8Intra,
							 kScalingListDefault8x8Inter,
							 kScalingListDefault8x8Intra,
							 kScalingListDefault8x8Inter]



-- derived entries (dictionary hack)
derivedFlatScalingLists = mkSynel "DERIVED_FlatScalingList" (SynelTypeUn 8)
derivedUseDefaultScalingMatrixFlag = mkSynel "DERIVED_UseDefaultScalingMatrixFlag" (SynelTypeUn 1)


parseScalingMatrix :: Synel -> Int -> (BitstreamBE, SynelDictionary) -> Maybe (BitstreamBE, SynelDictionary)
parseScalingMatrix lpfSynel listCount (bt, sd) =
	Just (bt, sd) >>=
	parseForEach [0..listCount-1] (\i (bt1, sd1) ->
		Just (bt1, sd1) >>=
		parse lpfSynel >>= \(bt11, sd11) ->
		(let
			isListPresent = (/=0) $ last $ sdArray sd11 lpfSynel
			listSize = if i < 6 then 16 else 64
		in
			if isListPresent then
				parseScalingList listSize (bt11, [], False) >>= \(bt12, vs, useDef) ->
				(let
					useDefInt = if useDef then 1 else 0
					sd12 = sdAppendToArray sd11 derivedUseDefaultScalingMatrixFlag [useDefInt]
					vs' = if useDef then replicate listSize 0 else vs
					sd13 = sdAppendToArray sd12 derivedFlatScalingLists vs'
				in
					return (bt12, sd13)
				)
			else
				(let
					sd12 = sdAppendToArray sd11 derivedUseDefaultScalingMatrixFlag [0]
					sd13 = sdAppendToArray sd12 derivedFlatScalingLists $ replicate listSize 0
				in
					return (bt11, sd13)
				)
		)
	)


-- spec 7.3.2.1.1.1
parseScalingList :: Int -> (BitstreamBE, [Int], Bool) -> Maybe (BitstreamBE, [Int], Bool)
parseScalingList n state = parseNextScale 8 8 0 n state

parseNextScale :: Int -> Int -> Int -> Int -> (BitstreamBE, [Int], Bool) -> Maybe (BitstreamBE, [Int], Bool)
parseNextScale _ _ _ 0 state = Just state
parseNextScale _ _ _ _ (bt, vs, True) = Just (bt, [], True)
parseNextScale lastSc nextSc idx r (bt, vs, useDef) =
	if nextSc /= 0 then
		parse synelDeltaScale (bt, emptySd) >>= \(bt1, sd1) ->
		(let
			delta = sdScalar sd1 synelDeltaScale
			nextSc' = (lastSc + delta + 256) `mod` 256
			useDef' = idx == 0 && nextSc' == 0
			scale = if nextSc' == 0 then lastSc else nextSc'
			vs' = vs ++ [if nextSc' == 0 then lastSc else nextSc']
		in
			parseNextScale scale nextSc' (idx+1) (r-1) (bt1, vs', useDef')
		)
	else
		(let
			vs' = vs ++ [lastSc]
		in
			parseNextScale lastSc nextSc (idx+1) (r-1) (bt, vs', useDef)
		)
	
	where
		synelDeltaScale = mkSynelV "delta_scale" SynelTypeSEv [0,1] (\x -> x >= -128 && x <= 127)


extractScalingLists :: Synel -> [[Int]] -> SynelDictionary -> Maybe [[Int]]
extractScalingLists lpfSynel fbMatrix sd
	| not (hasRequiredSynels && isValidData) = trace "ERROR: extractScalingLists: invalid input" Nothing
	| otherwise = Just $ foldr (extractList fbMatrix) [] [0..length listPresentFlags-1]
	where
		hasRequiredSynels = sdHasKeys sd [derivedFlatScalingLists,
							  			  derivedUseDefaultScalingMatrixFlag,
										  lpfSynel]
		flatList = sdArray sd derivedFlatScalingLists
		useDefFlags = sdArray sd derivedUseDefaultScalingMatrixFlag
		listPresentFlags = sdArray sd lpfSynel
		listCount = length listPresentFlags
		expectedFlatLength = if listCount <= 6 then listCount*16 else 6*16+(listCount-6)*64
		isValidData = length useDefFlags == listCount &&
					  length flatList == expectedFlatLength
		
		extractList :: [[Int]] -> Int -> [[Int]] -> [[Int]]
		extractList fbls i ls = ls ++ [list]
			where
				offset = if i <= 6 then i*16 else 6*16+(i-6)*64
				listSize = if i < 6 then 16 else 64
				list = if listPresentFlags !! i /= 0 then
						   if useDefFlags !! i /= 0 then
							   kDefaultScalingListMatrix !! i
						   else
							   take listSize $ drop offset flatList
					   else
						   fallbackRule !! i
				
				-- spec Table 7-2
				fallbackRule = [fbls !! 0,
								ls !! 0,
								ls !! 1,
								fbls !! 3,
								ls !! 3,
								ls !! 4,
								fbls !! 6,
								fbls !! 7,
								ls !! 6,
								ls !! 7,
								ls !! 8,
								ls !! 9]
