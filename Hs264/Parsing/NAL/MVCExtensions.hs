-- Hs264.Parsing.NAL.MVCExtensions

module Hs264.Parsing.NAL.MVCExtensions where

import Debug.Trace
import Hs264.Parsing.SyntaxElement


------------------------------------------------------------------------------
-- Multiview Video Coding NAL header extensions (spec Annex H)
------------------------------------------------------------------------------

data MvcHeader =
	MvcHeader {
		mvcNonIdrFlag :: Bool,
		mvcPriorityId :: Int,
		mvcViewId :: Int,
		mvcTemporalId :: Int,
		mvcAnchorPicFlag :: Bool,
		mvcInterViewFlag :: Bool
	} deriving (Eq, Show)
	
emptyMvc :: MvcHeader
emptyMvc =
	MvcHeader {
		mvcNonIdrFlag = False,
		mvcPriorityId = 0,
		mvcViewId = 0,
		mvcTemporalId = 0,
		mvcAnchorPicFlag = False,
		mvcInterViewFlag = False
	}

mvcFromDictionary :: SynelDictionary -> MvcHeader
mvcFromDictionary sd =
	MvcHeader {
		mvcNonIdrFlag = sdScalar sd synelNonIdrFlag /= 0,
		mvcPriorityId = sdScalar sd synelPriorityId,
		mvcViewId = sdScalar sd synelViewId,
		mvcTemporalId = sdScalar sd synelTemporalId,
		mvcAnchorPicFlag = sdScalar sd synelAnchorPicFlag /= 0,
		mvcInterViewFlag = sdScalar sd synelInterViewFlag /= 0
	}
		

-- spec H.7.4.1.1
synelNonIdrFlag = mkSynel "non_idr_flag" (SynelTypeUn 1)
synelPriorityId = mkSynel "priority_id" (SynelTypeUn 6)
synelViewId = mkSynel "view_id" (SynelTypeUn 10)
synelTemporalId = mkSynel "temporal_id" (SynelTypeUn 3)
synelAnchorPicFlag = mkSynel "anchor_pic_flag" (SynelTypeUn 1)
synelInterViewFlag = mkSynel "inter_view_flag" (SynelTypeUn 1)
synelReservedOneBit = mkSynelV "reserved_one_bit" (SynelTypeUn 1) (==1)

-- spec H.7.3.1.1
parseNalUnitHeaderMvcExtension :: BitstreamBE -> Maybe MvcHeader
parseNalUnitHeaderMvcExtension _ | trace "parseNalUnitHeaderMvcExtension" False = undefined
parseNalUnitHeaderMvcExtension bt =
	Just (bt, emptySd) >>=
	parse synelNonIdrFlag >>=
	parse synelPriorityId >>=
	parse synelViewId >>=
	parse synelTemporalId >>=
	parse synelAnchorPicFlag >>=
	parse synelInterViewFlag >>=
	parse synelReservedOneBit >>= \(bt1, sd1) ->
	return $ mvcFromDictionary sd1
		

