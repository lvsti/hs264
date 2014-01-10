-- Hs264.Parsing.RBSP.Internal

module Hs264.Parsing.RBSP.Internal where

import Data.Bits
import qualified Data.Bitstream.Lazy as BTL
import Data.Maybe
import Debug.Trace

import Hs264.Parsing.SyntaxElement


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

