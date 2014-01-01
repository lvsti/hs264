-- Hs264.Parsing.NAL.SVCExtensions

module Hs264.Parsing.NAL.SVCExtensions where

import Debug.Trace
import Hs264.Parsing.SyntaxElement


------------------------------------------------------------------------------
-- Scalable Video Coding NAL header extensions (spec Annex G)
------------------------------------------------------------------------------

data SvcHeader =
	SvcHeader {
		svcIdrFlag :: Bool,
		svcPriorityId :: Int,
		svcNoInterLayerPredFlag :: Bool,
		svcDependencyId :: Int,
		svcQualityId :: Int,
		svcTemporalId :: Int,
		svcUseRefBasePicFlag :: Bool,
		svcDiscardableFlag :: Bool,
		svcOutputFlag :: Bool
	} deriving (Eq, Show)

emptySvc :: SvcHeader
emptySvc =
	SvcHeader {
		svcIdrFlag = False,
		svcPriorityId = 0,
		svcNoInterLayerPredFlag = False,
		svcDependencyId = 0,
		svcQualityId = 0,
		svcTemporalId = 0,
		svcUseRefBasePicFlag = False,
		svcDiscardableFlag = False,
		svcOutputFlag = False
	}
				
svcFromDictionary :: SynelDictionary -> SvcHeader	   
svcFromDictionary sd =
	SvcHeader {
		svcIdrFlag = sdScalar sd synelIdrFlag /= 0,
		svcPriorityId = sdScalar sd synelPriorityId,
		svcNoInterLayerPredFlag = sdScalar sd synelNoInterLayerPredFlag /= 0,
		svcDependencyId = sdScalar sd synelDependencyId,
		svcQualityId = sdScalar sd synelQualityId,
		svcTemporalId = sdScalar sd synelTemporalId,
		svcUseRefBasePicFlag = sdScalar sd synelUseRefBasePicFlag /= 0,
		svcDiscardableFlag = sdScalar sd synelDiscardableFlag /= 0,
		svcOutputFlag = sdScalar sd synelOutputFlag /= 0
	}


-- spec G.7.4.1.1
synelIdrFlag = mkSynel "idr_flag" (SynelTypeUn 1)
synelPriorityId = mkSynel "priority_id" (SynelTypeUn 6)
synelNoInterLayerPredFlag = mkSynel "no_inter_layer_pred_flag" (SynelTypeUn 1)
synelDependencyId = mkSynel "dependency_id" (SynelTypeUn 3)
synelQualityId = mkSynel "quality_id" (SynelTypeUn 4)
synelTemporalId = mkSynel "temporal_id" (SynelTypeUn 3)
synelUseRefBasePicFlag = mkSynel "use_ref_base_pic_flag" (SynelTypeUn 1)
synelDiscardableFlag = mkSynel "discardable_flag" (SynelTypeUn 1)
synelOutputFlag = mkSynel "output_flag" (SynelTypeUn 1)
synelReservedThree2bits = mkSynelV "reserved_three_2bits" (SynelTypeUn 2) (==3)

-- spec G.7.3.1.1
parseNalUnitHeaderSvcExtension :: BitstreamBE -> Maybe SvcHeader
parseNalUnitHeaderSvcExtension _ | trace "parseNalUnitHeaderSvcExtension" False = undefined
parseNalUnitHeaderSvcExtension bt =
	Just (bt, emptySd) >>=
	parse synelIdrFlag >>=
	parse synelPriorityId >>=
	parse synelNoInterLayerPredFlag >>=
	parse synelDependencyId >>=
	parse synelQualityId >>=
	parse synelTemporalId >>=
	parse synelUseRefBasePicFlag >>=
	parse synelDiscardableFlag >>=
	parse synelOutputFlag >>=
	parse synelReservedThree2bits >>= \(bt1, sd1) ->
	return $ svcFromDictionary sd1
		
