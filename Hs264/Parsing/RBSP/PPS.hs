-- Hs264.Parsing.RBSP.PPS

module Hs264.Parsing.RBSP.PPS where

import Data.Bits
import qualified Data.Bitstream.Lazy as BTL
import Data.Maybe
import Debug.Trace

import Hs264.Parsing.RBSP.Internal
import Hs264.Parsing.SyntaxElement
import Hs264.Types.Context as CTX
import Hs264.Types.PPS
import Hs264.Types.SPS


-------------------------------------------------------------------------------
-- SPS syntax element parsing
-------------------------------------------------------------------------------

-- spec 7.3.2.2
parsePictureParameterSetRbsp :: H264Context -> BitstreamBE -> Maybe (BitstreamBE, PictureParameterSet)
parsePictureParameterSetRbsp _ _ | trace "parsePictureParameterSetRbsp" False = undefined
parsePictureParameterSetRbsp ctx bt =
	Just (bt, emptySd) >>=
	parse synelPicParameterSetId >>=
	parse synelSeqParameterSetId >>=
	parse synelEntropyCodingModeFlag >>=
	parse synelBottomFieldPicOrderInFramePresentFlag >>=
	parse synelNumSliceGroupsMinus1 >>= \(bt1, sd1) ->
	(let
		numSliceGroups = sdScalar sd1 synelNumSliceGroupsMinus1 + 1
	in
		if numSliceGroups > 0 then
			Just (bt1, sd1) >>=
			parse synelSliceGroupMapType >>= \(bt11, sd11) ->
			(let
				mapType = sdScalar sd11 synelSliceGroupMapType
			in
				case () of
					_ | mapType == 0 ->
					 	Just (bt11, sd11) >>=
						parseForEach [0..numSliceGroups-1] (\i (btl1, sdl1) ->
							Just (btl1, sdl1) >>=
							parse synelRunLengthMinus1
						)
					
					_ | mapType == 2 ->
					 	Just (bt11, sd11) >>=
						parseForEach [0..numSliceGroups-1] (\i (btl2, sdl2) ->
							Just (btl2, sdl2) >>=
							parse synelTopLeft >>=
							parse synelBottomRight
						)
					
					_ | mapType `elem` [3,4,5] ->
					 	Just (bt11, sd11) >>=
						parse synelSliceGroupChangeDirectionFlag >>=
						parse synelSliceGroupChangeRateMinus1
					
					_ | mapType == 6 ->
					 	Just (bt11, sd11) >>=
						parse synelPicSizeInMapUnitsMinus1 >>= \(bt111, sd111) ->
						(let
							picSizeInMU = sdScalar sd111 synelPicSizeInMapUnitsMinus1 + 1
							sgidSize = bitWidth numSliceGroups
						in
							Just (bt111, sd111) >>=
							parseForEach [0..picSizeInMU-1] (\i (btl3, sdl3) ->
								Just (btl3, sdl3) >>=
								parse (synelSliceGroupId_v sgidSize)
							)
						)
					
			)
		else
			Just (bt1, sd1)
	) >>=
	parse synelNumRefIdxL0DefaultActiveMinus1 >>=
	parse synelNumRefIdxL1DefaultActiveMinus1 >>=
	parse synelWeightedPredFlag >>=
	parse synelWeightedBipredIdc >>=
	parse synelPicInitQpMinus26 >>=
	parse synelPicInitQsMinus26 >>=
	parse synelChromaQpIndexOffset >>=
	parse synelDeblockingFilterControlPresentFlag >>=
	parse synelConstrainedIntraPredFlag >>=
	parse synelRedundantPicCntPresentFlag >>= \(bt2, sd2) ->
	checkForMoreRbspData bt2 >>= \(bt2, hasMoreData) ->
	(if hasMoreData then
		Just (bt2, sd2) >>=
		parse synelTransform8x8ModeFlag >>=
		parse synelPicScalingMatrixPresentFlag >>= \(bt21, sd21) ->
		(let
			hasScaling = sdScalar sd21 synelPicScalingMatrixPresentFlag /= 0
			tf8x8 = sdScalar sd21 synelTransform8x8ModeFlag /= 0
			spsId = sdScalar sd21 synelSeqParameterSetId
			sps = CTX.lookupSps spsId ctx
			chromaFormatIdc = spsChromaFormatIdc $ fromJust sps
			listCount = 6 + if tf8x8 then (if chromaFormatIdc == 3 then 6 else 2) else 0
		in
			if hasScaling then
				if isJust sps then
					Just (bt21, sd21) >>=
-- TODO: refactor the whole scaling list parsing
					parseForEach [0..listCount-1] (\i (btl1, sdl1) ->
						Just (btl1, sdl1) >>=
						parse synelPicScalingListPresentFlag >>= \(btl11, sdl11) ->
						(let
							isListPresent = (/=0) $ last $ sdArray sdl11 synelPicScalingListPresentFlag
							listSize = if i < 6 then 16 else 64
						in
							if isListPresent then
								parseScalingList listSize (btl11, [], False) >>= \(btl12, vs, useDef) ->
								(let
									useDefInt = if useDef then 1 else 0
									sdl12 = sdAppendToArray sdl11 derivedUseDefaultScalingMatrixFlag [useDefInt]
									vs' = if useDef then replicate listSize 0 else vs
									sdl13 = sdAppendToArray sdl12 derivedFlatScalingLists vs'
								in
									return (btl12, sdl13)
								)
							else
								(let
									sdl12 = sdAppendToArray sdl11 derivedUseDefaultScalingMatrixFlag [0]
									sdl13 = sdAppendToArray sdl12 derivedFlatScalingLists $ replicate listSize 0
								in
									return (btl11, sdl13)
								)
						)
					)
				else
					trace ("ERROR: parsePictureParameterSetRbsp: referenced SPS (" ++ show spsId ++ ") not found") Nothing
			else
				Just (bt21, sd21)
		) >>=
		parse synelSecondChromaQpIndexOffset
	else
		Just (bt2, sd2)
	) >>= \(bt3, sd3) ->
	parseRbspTrailingBits bt bt3 >>= \bt4 ->
	ppsFromDictionary ctx sd3 >>= \pps ->
	return (bt4, pps)


-- spec 7.4.2.2
synelPicParameterSetId = mkSynelV "pic_parameter_set_id" SynelTypeUEv (<=255)
synelSeqParameterSetId = mkSynelV "seq_parameter_set_id" SynelTypeUEv (<=31)
synelEntropyCodingModeFlag = mkSynel "entropy_coding_mode_flag" (SynelTypeUn 1)
synelBottomFieldPicOrderInFramePresentFlag = mkSynel "bottom_field_pic_order_in_frame_present_flag" (SynelTypeUn 1)
synelNumSliceGroupsMinus1 = mkSynelV "num_slice_groups_minus1" SynelTypeUEv (<=7) -- profile dependent
synelSliceGroupMapType = mkSynelV "slice_group_map_type" SynelTypeUEv (<=6)
synelRunLengthMinus1 = mkSynelA "run_length_minus1" SynelTypeUEv -- (<PicSizeInMapUnits)
synelTopLeft = mkSynelA "top_left" SynelTypeUEv -- (<bottom_right)
synelBottomRight = mkSynel "bottom_right" SynelTypeUEv -- (<PicSizeInMapUnits)
synelSliceGroupChangeDirectionFlag = mkSynelA "slice_group_change_direction_flag" (SynelTypeUn 1)
synelSliceGroupChangeRateMinus1 = mkSynel "slice_group_change_rate_minus1" SynelTypeUEv -- (<PicSizeInMapUnits)
synelPicSizeInMapUnitsMinus1 = mkSynel "pic_size_in_map_units_minus1" SynelTypeUEv
synelSliceGroupId_v = \n -> mkSynelA "slice_group_id" (SynelTypeUn n)-- runtime variable bit count -- (<NumSliceGroups)
synelNumRefIdxL0DefaultActiveMinus1 = mkSynelV "num_ref_idx_l0_default_active_minus1" SynelTypeUEv (<=31)
synelNumRefIdxL1DefaultActiveMinus1 = mkSynelV "num_ref_idx_l1_default_active_minus1" SynelTypeUEv (<=31)
synelWeightedPredFlag = mkSynel "weighted_pred_flag" (SynelTypeUn 1)
synelWeightedBipredIdc = mkSynelV "weighted_bipred_idc" (SynelTypeUn 2) (<=2)
synelPicInitQpMinus26 = mkSynelV "pic_init_qp_minus26" SynelTypeSEv (\x -> x >= -62 && x <= 25)
synelPicInitQsMinus26 = mkSynelV "pic_init_qs_minus26" SynelTypeSEv (\x -> x >= -26 && x <= 25)
synelChromaQpIndexOffset = mkSynelV "chroma_qp_index_offset" SynelTypeSEv (\x -> x >= -12 && x <= 12)
synelDeblockingFilterControlPresentFlag = mkSynel "deblocking_filter_control_present_flag" (SynelTypeUn 1)
synelConstrainedIntraPredFlag = mkSynel "constrained_intra_pred_flag" (SynelTypeUn 1)
synelRedundantPicCntPresentFlag = mkSynel "redundant_pic_cnt_present_flag" (SynelTypeUn 1)
synelTransform8x8ModeFlag = mkSynel "transform_8x8_mode_flag" (SynelTypeUn 1)
synelPicScalingMatrixPresentFlag = mkSynel "pic_scaling_matrix_present_flag" (SynelTypeUn 1)
synelPicScalingListPresentFlag = mkSynelA "pic_scaling_list_present_flag" (SynelTypeUn 1)
synelSecondChromaQpIndexOffset = mkSynelV "second_chroma_qp_index_offset" SynelTypeSEv (\x -> x >= -12 && x <= 12)

-- derived entries (dictionary hack)
derivedFlatScalingLists = mkSynelA "DERIVED_FlatScalingList" (SynelTypeUn 8)
derivedUseDefaultScalingMatrixFlag = mkSynelA "DERIVED_UseDefaultScalingMatrixFlag" (SynelTypeUn 1)



-------------------------------------------------------------------------------
-- Default values for PPS
-------------------------------------------------------------------------------

emptyPps :: PictureParameterSet
emptyPps = 
	PictureParameterSet {
		-- core parameters
		ppsPicParameterSetId = 0,
		ppsSeqParameterSetId = 0,
		ppsEntropyCodingModeFlag = False,
		ppsBottomFieldPicOrderInFramePresentFlag = False,
		ppsNumRefIdxL0DefaultActive = 1,
		ppsNumRefIdxL1DefaultActive = 1,
		ppsWeightedPredFlag = False,
		ppsWeightedBipredIdc = 0,
		ppsPicInitQp = 26,
		ppsPicInitQs = 26,
		ppsChromaQpIndexOffset = 0,
		ppsDeblockingFilterControlPresentFlag = False,
		ppsConstrainedIntraPredFlag = False,
		ppsRedundantPicCntPresentFlag = False,
		-- slice groups
		ppsNumSliceGroups = 1,
		ppsSliceGroupMapType = 0,
		ppsSliceGroupMapParameters = Nothing,
		-- optional parameters
		ppsTransform8x8ModeFlag = False,
		ppsPicScalingMatrixPresentFlag = False,
		ppsScalingLists = [],
		ppsSecondChromaQpIndexOffset = 0
	}


-------------------------------------------------------------------------------
-- Value semantics for parsed PPS syntax elements
-------------------------------------------------------------------------------

ppsFromDictionary :: H264Context -> SynelDictionary -> Maybe PictureParameterSet
ppsFromDictionary _ _ | trace "ppsFromDictionary" False = undefined
ppsFromDictionary ctx sd =
	Just emptyPps >>=
	setPpsCore sd >>=
	setPpsSliceGroups sd >>= \pps1 ->
	(let
		spsId = ppsSeqParameterSetId pps1
		sps = CTX.lookupSps spsId ctx
	in
		if isJust sps then
			Just pps1 >>=
			setPpsOptionalFields (fromJust sps) sd
		else
			Nothing
	)
		


setPpsCore :: SynelDictionary -> PictureParameterSet -> Maybe PictureParameterSet
setPpsCore sd pps
	| invalidInput = trace "ERROR: setPpsCore: invalid input" Nothing
	| otherwise = Just corePps
	where
		invalidInput = not hasRequiredSynels
		hasRequiredSynels = sdHasKeys sd [synelPicParameterSetId,
										  synelSeqParameterSetId,
										  synelEntropyCodingModeFlag,
										  synelBottomFieldPicOrderInFramePresentFlag,
										  synelNumRefIdxL0DefaultActiveMinus1,
										  synelNumRefIdxL1DefaultActiveMinus1,
										  synelWeightedPredFlag,
										  synelWeightedBipredIdc,
										  synelPicInitQpMinus26,
										  synelPicInitQsMinus26,
										  synelChromaQpIndexOffset,
										  synelDeblockingFilterControlPresentFlag,
										  synelConstrainedIntraPredFlag,
										  synelRedundantPicCntPresentFlag]
		corePps = pps {
			ppsPicParameterSetId = sdScalar sd synelPicParameterSetId,
			ppsSeqParameterSetId = sdScalar sd synelSeqParameterSetId,
			ppsEntropyCodingModeFlag = sdScalar sd synelEntropyCodingModeFlag /= 0,
			ppsBottomFieldPicOrderInFramePresentFlag = sdScalar sd synelBottomFieldPicOrderInFramePresentFlag /= 0,
			ppsNumRefIdxL0DefaultActive = sdScalar sd synelNumRefIdxL0DefaultActiveMinus1 + 1,
			ppsNumRefIdxL1DefaultActive = sdScalar sd synelNumRefIdxL1DefaultActiveMinus1 + 1,
			ppsWeightedPredFlag = sdScalar sd synelWeightedPredFlag /= 0,
			ppsWeightedBipredIdc = sdScalar sd synelWeightedBipredIdc,
			ppsPicInitQp = sdScalar sd synelPicInitQpMinus26,
			ppsPicInitQs = sdScalar sd synelPicInitQsMinus26,
			ppsChromaQpIndexOffset = sdScalar sd synelChromaQpIndexOffset,
			ppsDeblockingFilterControlPresentFlag = sdScalar sd synelDeblockingFilterControlPresentFlag /= 0,
			ppsConstrainedIntraPredFlag = sdScalar sd synelConstrainedIntraPredFlag /= 0,
			ppsRedundantPicCntPresentFlag = sdScalar sd synelRedundantPicCntPresentFlag /= 0
		}

setPpsSliceGroups :: SynelDictionary -> PictureParameterSet -> Maybe PictureParameterSet
setPpsSliceGroups sd pps
	| invalidInput = trace "ERROR: setPpsSliceGroups: invalid input" Nothing
	| otherwise = Just sgPps
	where
		invalidInput = not hasRequiredSynels ||
					   sgCount > 1 && (not hasMapTypeSynel ||
					   				   mapType == 0 && not hasMT0Synels ||
									   mapType == 2 && (not hasMT2Synels || not isValidMT2) ||
									   mapType `elem` [3,4,5] && not hasMT345Synels ||
									   mapType == 6 && (not hasMT6Synels ||
									   					mt6PicSizeInMU > 0 && not hasSgidSynels))
		hasRequiredSynels = sdHasKeys sd [synelNumSliceGroupsMinus1]
		sgCount = sdScalar sd synelNumSliceGroupsMinus1
		hasMapTypeSynel = sdHasKey sd synelSliceGroupMapType
		mapType = sdScalar sd synelSliceGroupMapType
		
		hasMT0Synels = sdHasKeys sd [synelRunLengthMinus1]
			
		hasMT2Synels = sdHasKeys sd [synelTopLeft, synelBottomRight]
		isValidMT2 = all (/=GT) $ zipWith compare (sgmTopLeft sgMap) (sgmBottomRight sgMap)
		
		hasMT345Synels = sdHasKeys sd [synelSliceGroupChangeDirectionFlag, synelSliceGroupChangeRateMinus1]
		hasMT6Synels = sdHasKeys sd [synelPicSizeInMapUnitsMinus1]
		mt6PicSizeInMU = sdScalar sd synelPicSizeInMapUnitsMinus1
		sgidSize = bitWidth sgCount
		hasSgidSynels = sdHasKeys sd [synelSliceGroupId_v sgidSize]
		sgMap = case () of
					_ | mapType == 0 ->
						SgmInterleaved {
							sgmRunLengthMinus1 = sdArray sd synelRunLengthMinus1
						}
					_ | mapType == 1 ->
						SgmDispersed
					_ | mapType == 2 ->
						SgmForegroundLeftover {
							sgmTopLeft = sdArray sd synelTopLeft,
							sgmBottomRight = sdArray sd synelBottomRight
						}
					_ | mapType `elem` [3,4,5] ->
						SgmChanging {
							sgmSliceGroupChangeDirectionFlag = sdScalar sd synelSliceGroupChangeDirectionFlag /= 0,
							sgmSliceGroupChangeRate = sdScalar sd synelSliceGroupChangeRateMinus1 + 1
						}
					_ | mapType == 6 ->
						SgmExplicit {
							sgmPicSizeInMapUnits = mt6PicSizeInMU,
							sgmSliceGroupId = if mt6PicSizeInMU > 0 then sdArray sd (synelSliceGroupId_v sgidSize) else []
						}
		sgPps = pps {
			ppsNumSliceGroups = sgCount,
			ppsSliceGroupMapType = mapType,
			ppsSliceGroupMapParameters = if sgCount > 1 then Just sgMap else Nothing
		}

setPpsOptionalFields :: SequenceParameterSet -> SynelDictionary -> PictureParameterSet -> Maybe PictureParameterSet
setPpsOptionalFields sps sd pps
	| invalidInput = trace "ERROR: setPpsOptionalFields: invalid input" Nothing
	| otherwise = Just optPps
	where
		invalidInput = not hasRequiredSynels ||
					   hasPicScalingMatrix && isNothing maybeMatrix
		hasRequiredSynels = sdHasKeys sd [synelTransform8x8ModeFlag,
										  synelSecondChromaQpIndexOffset,
										  synelPicScalingMatrixPresentFlag]
		hasPicScalingMatrix = sdScalar sd synelPicScalingMatrixPresentFlag /= 0
		hasSeqScalingMatrix = spsSeqScalingMatrixPresentFlag sps
		seqScalingLists = spsScalingLists sps
		maybeMatrix = if hasPicScalingMatrix then
						  extractPpsScalingLists sd
					  else
						  Just kInferredScalingListMatrix
		optPps = pps {
			ppsTransform8x8ModeFlag = sdScalar sd synelTransform8x8ModeFlag /= 0,
			ppsSecondChromaQpIndexOffset = sdScalar sd synelSecondChromaQpIndexOffset,
			ppsPicScalingMatrixPresentFlag = sdScalar sd synelPicScalingMatrixPresentFlag /= 0,
			ppsScalingLists = fromJust maybeMatrix
		}

		extractPpsScalingLists :: SynelDictionary -> Maybe [[Int]]
		extractPpsScalingLists sd
			| not (hasRequiredSynels && isValidData) = trace "ERROR: extractPpsScalingLists: invalid input" Nothing
			| otherwise = Just $ foldr extractPpsList [] [0..length listPresentFlags-1]
			where
				hasRequiredSynels = sdHasKeys sd [derivedFlatScalingLists,
									  			  derivedUseDefaultScalingMatrixFlag,
												  synelPicScalingListPresentFlag]
				flatList = sdArray sd derivedFlatScalingLists
				useDefFlags = sdArray sd derivedUseDefaultScalingMatrixFlag
				listPresentFlags = sdArray sd synelPicScalingListPresentFlag
				listCount = length listPresentFlags
				expectedFlatLength = if listCount <= 6 then listCount*16 else 6*16+(listCount-6)*64
				isValidData = length useDefFlags == listCount &&
							  length flatList == expectedFlatLength
				
				extractPpsList :: Int -> [[Int]] -> [[Int]]
				extractPpsList i ls = ls ++ [list]
					where
						offset = if i <= 6 then i*16 else 6*16+(i-6)*64
						listSize = if i < 6 then 16 else 64
						list = if listPresentFlags !! i /= 0 then
								   if useDefFlags !! i /= 0 then
									   kDefaultScalingListMatrix !! i
								   else
									   take listSize $ drop offset flatList
							   else
								   if hasSeqScalingMatrix then
									   fallbackB !! i
								   else
									   fallbackA !! i
						
						-- spec Table 7-2
						fallbackA = [kDefaultScalingListMatrix !! 0,
									 ls !! 0,
									 ls !! 1,
									 kDefaultScalingListMatrix !! 3,
									 ls !! 3,
									 ls !! 4,
									 kDefaultScalingListMatrix !! 6,
									 kDefaultScalingListMatrix !! 7,
									 ls !! 6,
									 ls !! 7,
									 ls !! 8,
									 ls !! 9]
						fallbackB = [seqScalingLists !! 0,
									 ls !! 0,
									 ls !! 1,
									 seqScalingLists !! 3,
									 ls !! 3,
									 ls !! 4,
									 seqScalingLists !! 6,
									 seqScalingLists !! 7,
									 ls !! 6,
									 ls !! 7,
									 ls !! 8,
									 ls !! 9]

