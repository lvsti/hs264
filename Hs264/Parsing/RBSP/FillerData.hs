-- Hs264.Parsing.RBSP.FillerData

module Hs264.Parsing.RBSP.FillerData where

import Data.Bits
import qualified Data.Bitstream.Lazy as BTL
import Data.Maybe
import Debug.Trace

import Hs264.Parsing.RBSP.Internal
import Hs264.Parsing.SyntaxElement
import Hs264.Types.Context as CTX


peekBits :: Int -> BitstreamBE -> Int
peekBits n bt = (BTL.toBits :: BitstreamBE -> Int) $ BTL.take n bt


-------------------------------------------------------------------------------
-- Filler Data RBSP parsing
-------------------------------------------------------------------------------

-- spec 7.3.2.7
parseFillerDataRbsp :: H264Context -> BitstreamBE -> Maybe (BitstreamBE, H264Context)
parseFillerDataRbsp _ _ | trace "parseFillerDataRbsp" False = undefined
parseFillerDataRbsp ctx bt =
	Just (bt, emptySd) >>=
	parseWhile (\(btp, _) -> BTL.length btp > 8 && peekBits 8 btp == 0xff) (\(btl1, sdl1) ->
		Just (btl1, sdl1) >>=
		parse synelFfByte
	) >>= \(bt1, _) ->
	parseRbspTrailingBits bt bt1 >>= \bt2 ->
	return (bt2, ctx)


-- spec 7.4.2.7
synelFfByte = mkSynelV "ff_byte" (SynelTypeFn 8) (==0xff)

