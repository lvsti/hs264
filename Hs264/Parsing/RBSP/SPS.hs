-- Hs264.Parsing.RBSP.SPS

module Hs264.Parsing.RBSP.SPS where

import Data.Bits
import qualified Data.Bitstream.Lazy as BTL
import Data.Maybe
import Debug.Trace

import Hs264.Parsing.RBSP.Internal
import Hs264.Parsing.RBSP.SPS.VUI
import Hs264.Parsing.SyntaxElement
import Hs264.Types.Context as CTX
import Hs264.Types.SPS



-------------------------------------------------------------------------------
-- SPS syntax element parsing
-------------------------------------------------------------------------------


-- spec 7.3.2.1.1
parseSequenceParameterSetRbsp :: H264Context -> BitstreamBE -> Maybe (BitstreamBE, SequenceParameterSet)
parseSequenceParameterSetRbsp _ _ | trace "parseSequenceParameterSetRbsp" False = undefined
parseSequenceParameterSetRbsp ctx bt =
	Just (bt, emptySd) >>=
	parse synelProfileIdc >>=
	parse synelConstraintSet0Flag >>=
	parse synelConstraintSet1Flag >>=
	parse synelConstraintSet2Flag >>=
	parse synelConstraintSet3Flag >>=
	parse synelConstraintSet4Flag >>=
	parse synelConstraintSet5Flag >>=
	parse synelReservedZero2bits >>=
	parse synelLevelIdc >>=
	parse synelSeqParameterSetId >>= \(bt1, sd1) ->
	(let
		profileIdc = sdScalar sd1 synelProfileIdc
		hasColorInfo = profileIdc `elem` [100,110,122,244,44,83,86,118,128,138]
	in
		if hasColorInfo then
			Just (bt1, sd1) >>=
			parse synelChromaFormatIdc >>= \(bt12, sd12) ->
			(let
				chromaFormatIdc = sdScalar sd12 synelChromaFormatIdc
			in
				if chromaFormatIdc == 3 then
					Just (bt12, sd12) >>=
					parse synelSeparateColourPlaneFlag
				else
					Just (bt12, sd12)
			) >>=
			parse synelBitDepthLumaMinus8 >>=
			parse synelBitDepthChromaMinus8 >>=
			parse synelQpPrimeYZeroTransformBypassFlag >>=
			parse synelSeqScalingMatrixPresentFlag >>= \(bt13, sd13) ->
			(let
				hasScaling = sdScalar sd13 synelSeqScalingMatrixPresentFlag /= 0
				chromaFormatIdc = sdScalar sd13 synelChromaFormatIdc
				listCount = if chromaFormatIdc == 3 then 12 else 8
			in
				if hasScaling then
					Just (bt13, sd13) >>=
					parseScalingMatrix synelSeqScalingListPresentFlag listCount
				else
					Just (bt13, sd13)
			)
		else
			Just (bt1, sd1)
	) >>=
	parse synelLog2MaxFrameNumMinus4 >>=
	parse synelPicOrderCntType >>= \(bt2, sd2) ->
	(let
		picOrderCntType = sdScalar sd2 synelPicOrderCntType
	in
		if picOrderCntType == 0 then
			Just (bt2, sd2) >>=
			parse synelLog2MaxPicOrderCntLsbMinus4
		else
			if picOrderCntType == 1 then
				Just (bt2, sd2) >>=
				parse synelDeltaPicOrderAlwaysZeroFlag >>=
				parse synelOffsetForNonRefPic >>=
				parse synelOffsetForTopToBottomField >>=
				parse synelNumRefFramesInPicOrderCntCycle >>= \(bt21, sd21) ->
				(let
					numRefFramesInPoc = sdScalar sd21 synelNumRefFramesInPicOrderCntCycle
				in
					if numRefFramesInPoc > 0 then
						Just (bt21, sd21) >>=
						parseForEach [0..numRefFramesInPoc-1] (\_ (bt211, sd211) ->
							Just (bt211, sd211) >>=
							parse synelOffsetForRefFrame
						)
					else
						Just (bt21, sd21)
				)	
			else
				Just (bt2, sd2)
	) >>=
	parse synelMaxNumRefFrames >>=
	parse synelGapsInFrameNumValueAllowedFlag >>=
	parse synelPicWidthInMbsMinus1 >>=
	parse synelPicHeightInMapUnitsMinus1 >>=
	parse synelFrameMbsOnlyFlag >>= \(bt3, sd3) ->
	(let
		frameMbsOnly = sdScalar sd3 synelFrameMbsOnlyFlag /= 0
	in
		if frameMbsOnly then
			Just (bt3, sd3)
		else
			Just (bt3, sd3) >>=
			parse synelMbAdaptiveFrameFieldFlag
	) >>=
	parse synelDirect8x8InferenceFlag >>=
	parse synelFrameCroppingFlag >>= \(bt4, sd4) ->
	(let
		needsCropping = sdScalar sd4 synelFrameCroppingFlag /= 0
	in
		if needsCropping then
			Just (bt4, sd4) >>=
			parse synelFrameCropLeftOffset >>=
			parse synelFrameCropRightOffset >>=
			parse synelFrameCropTopOffset >>=
			parse synelFrameCropBottomOffset
		else
			Just (bt4, sd4)
	) >>=
	parse synelVuiParametersPresentFlag >>= \(bt5, sd5) ->
	(let
		hasVuiParams = sdScalar sd5 synelVuiParametersPresentFlag /= 0
	in
		if hasVuiParams then
			Just (bt5, sd5) >>=
			parseVuiParameters
		else
			Just (bt5, sd5)
	) >>= \(bt6, sd6) ->
	parseRbspTrailingBits bt bt6 >>= \bt7 ->
	spsFromDictionary ctx sd6 >>= \sps ->
	return (bt7, sps)
		

-- spec 7.4.2.1.1
synelProfileIdc = mkSynel "profile_idc" (SynelTypeUn 8)
synelConstraintSet0Flag = mkSynel "constraint_set0_flag" (SynelTypeUn 1)
synelConstraintSet1Flag = mkSynel "constraint_set1_flag" (SynelTypeUn 1)
synelConstraintSet2Flag = mkSynel "constraint_set2_flag" (SynelTypeUn 1)
synelConstraintSet3Flag = mkSynel "constraint_set3_flag" (SynelTypeUn 1)
synelConstraintSet4Flag = mkSynel "constraint_set4_flag" (SynelTypeUn 1)
synelConstraintSet5Flag = mkSynel "constraint_set5_flag" (SynelTypeUn 1)
synelReservedZero2bits = mkSynelV "reserved_zero_2bits" (SynelTypeUn 2) (==0)
synelLevelIdc = mkSynel "level_idc" (SynelTypeUn 8)
synelSeqParameterSetId = mkSynelV "seq_parameter_set_id" SynelTypeUEv (<=31)
synelChromaFormatIdc = mkSynelV "chroma_format_idc" SynelTypeUEv (<=3)
synelSeparateColourPlaneFlag = mkSynel "separate_colour_plane_flag" (SynelTypeUn 1)
synelBitDepthLumaMinus8 = mkSynelV "bit_depth_luma_minus8" SynelTypeUEv (<=6)
synelBitDepthChromaMinus8 = mkSynelV "bit_depth_chroma_minus8" SynelTypeUEv (<=6)
synelQpPrimeYZeroTransformBypassFlag = mkSynel "qpprime_y_zero_transform_bypass_flag" (SynelTypeUn 1)
synelSeqScalingMatrixPresentFlag = mkSynel "seq_scaling_matrix_present_flag" (SynelTypeUn 1)
synelSeqScalingListPresentFlag = mkSynelA "seq_scaling_list_present_flag" (SynelTypeUn 1)
synelLog2MaxFrameNumMinus4 = mkSynelV "log2_max_frame_num_minus4" SynelTypeUEv (<=12)
synelPicOrderCntType = mkSynelV "pic_order_cnt_type" SynelTypeUEv (<=2)
synelLog2MaxPicOrderCntLsbMinus4 = mkSynelV "log2_max_pic_order_cnt_lsb_minus4" SynelTypeUEv (<=12)
synelDeltaPicOrderAlwaysZeroFlag = mkSynel "delta_pic_order_always_zero_flag" (SynelTypeUn 1)
synelOffsetForNonRefPic = mkSynelV "offset_for_non_ref_pic" SynelTypeSEv (\x -> x .&. 0xffffffff == x)
synelOffsetForTopToBottomField = mkSynelV "offset_for_top_to_bottom_field" SynelTypeSEv (\x -> x .&. 0xffffffff == x)
synelNumRefFramesInPicOrderCntCycle = mkSynelV "num_ref_frames_in_pic_order_cnt_cycle" SynelTypeUEv (<=255)
synelOffsetForRefFrame = mkSynelAV "offset_for_ref_frame" SynelTypeSEv (\x -> x .&. 0xffffffff == x)
synelMaxNumRefFrames = mkSynel "max_num_ref_frames" SynelTypeUEv -- (<=MaxDpbFrames)
synelGapsInFrameNumValueAllowedFlag = mkSynel "gaps_in_frame_num_value_allowed_flag" (SynelTypeUn 1)
synelPicWidthInMbsMinus1 = mkSynel "pic_width_in_mbs_minus1" SynelTypeUEv
synelPicHeightInMapUnitsMinus1 = mkSynel "pic_height_in_map_units_minus1" SynelTypeUEv
synelFrameMbsOnlyFlag = mkSynel "frame_mbs_only_flag" (SynelTypeUn 1)
synelMbAdaptiveFrameFieldFlag = mkSynel "mb_adaptive_frame_field_flag" (SynelTypeUn 1)
synelDirect8x8InferenceFlag = mkSynel "direct_8x8_inference_flag" (SynelTypeUn 1)
synelFrameCroppingFlag = mkSynel "frame_cropping_flag" (SynelTypeUn 1)
synelFrameCropLeftOffset = mkSynel "frame_crop_left_offset" SynelTypeUEv
synelFrameCropRightOffset = mkSynel "frame_crop_right_offset" SynelTypeUEv
synelFrameCropTopOffset = mkSynel "frame_crop_top_offset" SynelTypeUEv
synelFrameCropBottomOffset = mkSynel "frame_crop_bottom_offset" SynelTypeUEv
synelVuiParametersPresentFlag = mkSynel "vui_parameters_present_flag" (SynelTypeUn 1)



-------------------------------------------------------------------------------
-- Default values for SPS
-------------------------------------------------------------------------------

emptySps :: SequenceParameterSet
emptySps =
	SequenceParameterSet {
		spsProfileIdc = 0,
		spsConstraintSetFlags = replicate 6 False,
		spsLevelIdc = 0,
		
		spsChromaFormatIdc = 1,
		spsSeparateColourPlaneFlag = False,
		spsChromaArrayType = 1,
		spsBitDepthY = 8,
		spsBitDepthC = 8,
		spsQpBdOffsetY = 0,
		spsQpBdOffsetC = 0,
		
		spsQpPrimeYZeroTransformBypassFlag = False,
		spsSeqScalingMatrixPresentFlag = False,
		spsScalingLists = kInferredScalingListMatrix,
		
		spsMaxFrameNum = 0,
		spsPicOrderCntType = 0,
		spsMaxPicOrderCntLsb = 0,
		spsDeltaPicOrderAlwaysZeroFlag = False,
		spsOffsetForNonRefPic = 0,
		spsOffsetForTopToBottomField = 0,
		spsNumRefFramesInPicOrderCntCycle = 0,
		spsRefFrameOffsets = [],
		spsMaxNumRefFrames = 0,
		spsGapsInFrameNumValueAllowedFlag = False,
		
		spsPicWidthInMbs = 0,
		spsPicHeightInMapUnits = 0,
		spsFrameCroppingFlag = False,
		spsFrameCropLeftOffset = 0,
		spsFrameCropRightOffset = 0,
		spsFrameCropTopOffset = 0,
		spsFrameCropBottomOffset = 0,
		
		spsSeqParameterSetId = 0,
		spsFrameMbsOnlyFlag = False,
		spsMbAdaptiveFrameFieldFlag = False,
		spsDirect8x8InferenceFlag = False,
		
		spsVuiParametersPresentFlag = False,
		spsVuiParameters = Nothing
	}


-------------------------------------------------------------------------------
-- Value semantics for parsed SPS syntax elements
-------------------------------------------------------------------------------

spsFromDictionary :: H264Context -> SynelDictionary -> Maybe SequenceParameterSet
spsFromDictionary _ _ | trace "spsFromDictionary" False = undefined
spsFromDictionary ctx sd =
	Just emptySps >>=
	setSpsProfilesAndLevels sd >>=
	setSpsColorAndTransform sd >>=
	setSpsFrameOrdering sd >>=
	setSpsFrameDimensions sd >>=
	setSpsMiscellaneous sd >>=
	setSpsVui sd


setSpsProfilesAndLevels :: SynelDictionary -> SequenceParameterSet -> Maybe SequenceParameterSet
setSpsProfilesAndLevels sd sps
	| not hasRequiredSynels = trace "ERROR: setSpsProfilesAndLevels: invalid input" Nothing
	| otherwise = if hasValidProfilesAndLevels False rawSps then
					  if hasValidProfilesAndLevels True rawSps then
						  Just rawSps
					  else
						  trace "WARNING: setSpsProfilesAndLevels: strict validation failed" $ Just rawSps
				  else
					  Nothing
	where
		hasRequiredSynels = sdHasKeys sd [synelProfileIdc,
							  			  synelConstraintSet0Flag,
										  synelConstraintSet1Flag,
										  synelConstraintSet2Flag,
										  synelConstraintSet3Flag,
										  synelConstraintSet4Flag,
										  synelConstraintSet5Flag,
										  synelLevelIdc]
		rawSps = sps {
			spsProfileIdc = sdScalar sd synelProfileIdc,
			spsConstraintSetFlags = [sdScalar sd synelConstraintSet0Flag /= 0,
									 sdScalar sd synelConstraintSet1Flag /= 0,
									 sdScalar sd synelConstraintSet2Flag /= 0,
									 sdScalar sd synelConstraintSet3Flag /= 0,
									 sdScalar sd synelConstraintSet4Flag /= 0,
									 sdScalar sd synelConstraintSet5Flag /= 0],
			spsLevelIdc = sdScalar sd synelLevelIdc
		}

		hasValidProfilesAndLevels :: Bool -> SequenceParameterSet -> Bool
		hasValidProfilesAndLevels strict sps =
			(profile /= 44 || (flags !! 3)) &&
			(not strict || not (profile `elem` [66,77,88] && level /= 11) || not (flags !! 3)) &&
			(not strict || profile `elem` [77,88,100,118,128] || not (flags !! 4)) &&
			(not strict || profile `elem` [77,88,100,118] || not (flags !! 5))
			where
				profile = spsProfileIdc sps
				level = spsLevelIdc sps
				flags = spsConstraintSetFlags sps


setSpsColorAndTransform :: SynelDictionary -> SequenceParameterSet -> Maybe SequenceParameterSet
setSpsColorAndTransform sd sps
	| hasMissingFields = trace "ERROR: setSpsColorAndTransform: invalid input" Nothing
	| otherwise = Just $ if hasColorInfo then colorSps else sps
	where
		hasMissingFields = not (sdHasKey sd synelProfileIdc) ||
						   hasColorInfo && (not hasRequiredSynels ||
						   					chromaFormatIdc == 3 && not (sdHasKey sd synelSeparateColourPlaneFlag) ||
											not (isJust maybeMatrix))
		hasColorInfo = (sdScalar sd synelProfileIdc) `elem` [100,110,122,244,44,83,86,118,128,138]
		hasRequiredSynels = sdHasKeys sd [synelChromaFormatIdc,
							  			  synelBitDepthLumaMinus8,
										  synelBitDepthChromaMinus8,
										  synelQpPrimeYZeroTransformBypassFlag,
										  synelSeqScalingMatrixPresentFlag]
		chromaFormatIdc = sdScalar sd synelChromaFormatIdc
		bdLumaM8 = sdScalar sd synelBitDepthLumaMinus8
		bdChromaM8 = sdScalar sd synelBitDepthChromaMinus8
		separateCP = if chromaFormatIdc == 3 then sdScalar sd synelSeparateColourPlaneFlag /= 0 else False
		hasScalingMatrix = sdScalar sd synelSeqScalingMatrixPresentFlag /= 0
		fallbackMatrix = kDefaultScalingListMatrix
		maybeMatrix = if hasScalingMatrix then
						  extractScalingLists synelSeqScalingListPresentFlag fallbackMatrix sd
					  else
						  Just kInferredScalingListMatrix
		colorSps = sps {
			spsChromaFormatIdc = chromaFormatIdc,
			spsSeparateColourPlaneFlag = separateCP,
			spsChromaArrayType = if separateCP then 0 else chromaFormatIdc,
			spsBitDepthY = bdLumaM8 + 8,
			spsQpBdOffsetY = bdLumaM8 * 6,
			spsBitDepthC = bdChromaM8 + 8,
			spsQpBdOffsetC = bdChromaM8 * 6,
			spsQpPrimeYZeroTransformBypassFlag = sdScalar sd synelQpPrimeYZeroTransformBypassFlag /= 0,
			spsSeqScalingMatrixPresentFlag = hasScalingMatrix,
			spsScalingLists = fromJust maybeMatrix
		}


setSpsFrameOrdering :: SynelDictionary -> SequenceParameterSet -> Maybe SequenceParameterSet
setSpsFrameOrdering sd sps
	| hasMissingFields = trace "ERROR: setSpsFrameOrdering: invalid input" Nothing
	| otherwise = Just $ [poc0Sps, poc1Sps, poc2Sps] !! pocType
	where
		hasMissingFields = not hasRequiredSynels ||
						   (pocType == 0 && not hasPoc0RequiredSynels) ||
						   (pocType == 1 && not (hasPoc1RequiredSynels && hasValidFrameOffsets))
		hasRequiredSynels = sdHasKeys sd [synelPicOrderCntType,
										  synelLog2MaxFrameNumMinus4,
										  synelMaxNumRefFrames,
										  synelGapsInFrameNumValueAllowedFlag]
		hasPoc0RequiredSynels = sdHasKeys sd [synelLog2MaxPicOrderCntLsbMinus4]
		hasPoc1RequiredSynels = sdHasKeys sd [synelDeltaPicOrderAlwaysZeroFlag,
											  synelOffsetForNonRefPic,
											  synelOffsetForTopToBottomField,
											  synelNumRefFramesInPicOrderCntCycle]
		hasValidFrameOffsets = numRefFrames == 0 ||
							   sdHasKey sd synelOffsetForRefFrame && length rfOffsets == numRefFrames
		
		pocType = sdScalar sd synelPicOrderCntType
		commonSps = sps {
			spsMaxFrameNum = 1 `shiftL` (sdScalar sd synelLog2MaxFrameNumMinus4 + 4),
			spsPicOrderCntType = pocType,
			spsMaxNumRefFrames = sdScalar sd synelMaxNumRefFrames,
			spsGapsInFrameNumValueAllowedFlag = sdScalar sd synelGapsInFrameNumValueAllowedFlag /= 0
		}
		poc0Sps = commonSps {
			spsMaxPicOrderCntLsb = 1 `shiftL` (sdScalar sd synelLog2MaxPicOrderCntLsbMinus4 + 4)
		}
		numRefFrames = sdScalar sd synelNumRefFramesInPicOrderCntCycle
		rfOffsets = sdArray sd synelOffsetForRefFrame
		poc1Sps = commonSps {
			spsDeltaPicOrderAlwaysZeroFlag = sdScalar sd synelDeltaPicOrderAlwaysZeroFlag /= 0,
			spsOffsetForNonRefPic = sdScalar sd synelOffsetForNonRefPic,
			spsOffsetForTopToBottomField = sdScalar sd synelOffsetForTopToBottomField,
			spsNumRefFramesInPicOrderCntCycle = numRefFrames,
			spsRefFrameOffsets = rfOffsets
		}
		poc2Sps = commonSps


setSpsFrameDimensions :: SynelDictionary -> SequenceParameterSet -> Maybe SequenceParameterSet
setSpsFrameDimensions sd sps
	| hasMissingFields = trace "ERROR: setSpsFrameDimensions: invalid input" Nothing
	| otherwise = Just $ if hasCropping then croppingSps else commonSps
	where
		hasMissingFields = not hasRequiredSynels ||
						   hasCropping && not hasCroppingSynels
		hasRequiredSynels = sdHasKeys sd [synelFrameCroppingFlag,
										  synelPicWidthInMbsMinus1,
										  synelPicHeightInMapUnitsMinus1]
		hasCroppingSynels = sdHasKeys sd [synelFrameCropLeftOffset,
										  synelFrameCropRightOffset,
										  synelFrameCropTopOffset,
										  synelFrameCropBottomOffset]
		hasCropping = sdScalar sd synelFrameCroppingFlag /= 0
		commonSps = sps {
			spsPicWidthInMbs = sdScalar sd synelPicWidthInMbsMinus1 + 1,
			spsPicHeightInMapUnits = sdScalar sd synelPicHeightInMapUnitsMinus1 + 1,
			spsFrameCroppingFlag = hasCropping
		}
		croppingSps = commonSps {
			spsFrameCropLeftOffset = sdScalar sd synelFrameCropLeftOffset,
			spsFrameCropRightOffset = sdScalar sd synelFrameCropRightOffset,
			spsFrameCropTopOffset = sdScalar sd synelFrameCropTopOffset,
			spsFrameCropBottomOffset = sdScalar sd synelFrameCropBottomOffset
		}


setSpsMiscellaneous :: SynelDictionary -> SequenceParameterSet -> Maybe SequenceParameterSet
setSpsMiscellaneous sd sps
	| hasMissingFields = trace "ERROR: setSpsMiscellaneous: invalid input" Nothing
	| otherwise = Just rawSps
	where
		hasMissingFields = not hasRequiredSynels ||
						   not frameMbsOnly && not (sdHasKey sd synelMbAdaptiveFrameFieldFlag)
		hasRequiredSynels = sdHasKeys sd [synelFrameMbsOnlyFlag,
										  synelSeqParameterSetId,
										  synelDirect8x8InferenceFlag]
		frameMbsOnly = sdScalar sd synelFrameMbsOnlyFlag /= 0
		rawSps = sps {
			spsSeqParameterSetId = sdScalar sd synelSeqParameterSetId,
			spsFrameMbsOnlyFlag = frameMbsOnly,
			spsMbAdaptiveFrameFieldFlag = if frameMbsOnly then False else (sdScalar sd synelMbAdaptiveFrameFieldFlag /= 0),
			spsDirect8x8InferenceFlag = sdScalar sd synelDirect8x8InferenceFlag /= 0
		}


setSpsVui :: SynelDictionary -> SequenceParameterSet -> Maybe SequenceParameterSet
setSpsVui sd sps
	| hasMissingFields = trace "ERROR: setSpsVui: invalid input" Nothing
	| otherwise = Just commonSps
	where
		hasMissingFields = not hasRequiredSynels ||
						   hasVui && not (isJust vui)
		hasRequiredSynels = sdHasKeys sd [synelVuiParametersPresentFlag]
		hasVui = sdScalar sd synelVuiParametersPresentFlag /= 0
		vui = vuiFromDictionary sd
		commonSps = sps {
			spsVuiParametersPresentFlag = hasVui,
			spsVuiParameters = if hasVui then vui else Nothing
		}

