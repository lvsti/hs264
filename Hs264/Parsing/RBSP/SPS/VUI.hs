-- Hs264.Parsing.RBSP.SPS.VUI

module Hs264.Parsing.RBSP.SPS.VUI where

import Data.Bits
import qualified Data.Bitstream.Lazy as BTL
import Debug.Trace

import Hs264.Parsing.SyntaxElement
import Hs264.Types.VUI

------------------------------------------------------------------------------
-- Video Usability Information (spec Annex E)
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- VUI Parameters
------------------------------------------------------------------------------

emptyVui :: VuiParameters
emptyVui = VuiParameters
	-- TODO

vuiFromDictionary :: SynelDictionary -> Maybe VuiParameters
vuiFromDictionary sd = Just emptyVui
	-- TODO



-- spec E.2.1
synelAspectRatioInfoPresentFlag = mkSynel "aspect_ratio_info_present_flag" (SynelTypeUn 1)
synelAspectRatioIdc = mkSynel "aspect_ratio_idc" (SynelTypeUn 8)
synelSarWidth = mkSynel "sar_width" (SynelTypeUn 16)
synelSarHeight = mkSynel "sar_height" (SynelTypeUn 16)
synelOverscanInfoPresentFlag = mkSynel "overscan_info_present_flag" (SynelTypeUn 1)
synelOverscanAppropriateFlag = mkSynel "overscan_appropriate_flag" (SynelTypeUn 1)
synelVideoSignalTypePresentFlag = mkSynel "video_signal_type_present_flag" (SynelTypeUn 1)
synelVideoFormat = mkSynel "video_format" (SynelTypeUn 3)
synelVideoFullRangeFlag = mkSynel "video_full_range_flag" (SynelTypeUn 1)
synelColourDescriptionPresentFlag = mkSynel "colour_description_present_flag" (SynelTypeUn 1)
synelColourPrimaries = mkSynel "colour_primaries" (SynelTypeUn 8)
synelTransferCharacteristics = mkSynel "transfer_characteristics" (SynelTypeUn 8)
synelMatrixCoefficients = mkSynel "matrix_coefficients" (SynelTypeUn 8)
synelChromaLocInfoPresentFlag = mkSynel "chroma_loc_info_present_flag" (SynelTypeUn 1)
synelChromaSampleLocTypeTopField = mkSynelV "chroma_sample_loc_type_top_field" SynelTypeUEv (<=5)
synelChromaSampleLocTypeBottomField = mkSynelV "chroma_sample_loc_type_bottom_field" SynelTypeUEv (<=5)
synelTimingInfoPresentFlag = mkSynel "timing_info_present_flag" (SynelTypeUn 1)
synelNumUnitsInTick = mkSynel "num_units_in_tick" (SynelTypeUn 32)
synelTimeScale = mkSynel "time_scale" (SynelTypeUn 32)
synelFixedFrameRateFlag = mkSynel "fixed_frame_rate_flag" (SynelTypeUn 1)
synelNalHrdParametersPresentFlag = mkSynel "nal_hrd_parameters_present_flag" (SynelTypeUn 1)
synelVclHrdParametersPresentFlag = mkSynel "vcl_hrd_parameters_present_flag" (SynelTypeUn 1)
synelLowDelayHrdFlag = mkSynel "low_delay_hrd_flag" (SynelTypeUn 1)
synelPicStructPresentFlag = mkSynel "pic_struct_present_flag" (SynelTypeUn 1)
synelBitstreamRestrictionFlag = mkSynel "bitstream_restriction_flag" (SynelTypeUn 1)
synelMotionVectorsOverPicBoundariesFlag = mkSynel "motion_vectors_over_pic_boundaries_flag" (SynelTypeUn 1)
synelMaxBytesPerPicDenom = mkSynelV "max_bytes_per_pic_denom" SynelTypeUEv (<=16)
synelMaxBitsPerMbDenom = mkSynelV "max_bits_per_mb_denom" SynelTypeUEv (<=16)
synelLog2MaxMvLengthHorizontal = mkSynelV "log2_max_mv_length_horizontal" SynelTypeUEv (<=16)
synelLog2MaxMvLengthVertical = mkSynelV "log2_max_mv_length_vertical" SynelTypeUEv (<=16)
synelMaxNumReorderFrames = mkSynel "max_num_reorder_frames" SynelTypeUEv -- (<=max_dec_frame_buffering)
synelMaxDecFrameBuffering = mkSynel "max_dec_frame_buffering" SynelTypeUEv -- (>=max_num_ref_frames)


-- spec E.1.1
parseVuiParameters :: (BitstreamBE, SynelDictionary) -> Maybe (BitstreamBE, SynelDictionary)
parseVuiParameters _ | trace "parseVuiParameters" False = undefined
parseVuiParameters (bt, sd) =
	Just (bt, sd) >>=
	parse synelAspectRatioInfoPresentFlag >>= \(bt1, sd1) ->
	(let
		hasARInfo = sdScalar sd1 synelAspectRatioInfoPresentFlag /= 0
	in
		if hasARInfo then
			Just (bt1, sd1) >>=
			parse synelAspectRatioIdc >>= \(bt11, sd11) ->
			(let
				arIdc = sdScalar sd11 synelAspectRatioIdc
				kExtendedSar = 255
			in
				if arIdc == kExtendedSar then
					Just (bt11, sd11) >>=
					parse synelSarWidth >>=
					parse synelSarHeight
				else
					Just (bt11, sd11)
			)
		else
			Just (bt1, sd1)
	) >>=
	parse synelOverscanInfoPresentFlag >>= \(bt2, sd2) ->
	(let
		hasOSInfo = sdScalar sd2 synelOverscanInfoPresentFlag /= 0
	in
		if hasOSInfo then
			Just (bt2, sd2) >>=
			parse synelOverscanAppropriateFlag
		else
			Just (bt2, sd2)
	) >>=
	parse synelVideoSignalTypePresentFlag >>= \(bt3, sd3) ->
	(let
		hasVst = sdScalar sd3 synelVideoSignalTypePresentFlag /= 0
	in
		if hasVst then
			Just (bt3, sd3) >>=
			parse synelVideoFormat >>=
			parse synelVideoFullRangeFlag >>=
			parse synelColourDescriptionPresentFlag >>= \(bt31, sd31) ->
			(let
				hasColourDesc = sdScalar sd31 synelColourDescriptionPresentFlag /= 0
			in
				if hasColourDesc then
					Just (bt31, sd31) >>=
					parse synelColourPrimaries >>=
					parse synelTransferCharacteristics >>=
					parse synelMatrixCoefficients
				else
					Just (bt31, sd31)
			)
		else
			Just (bt3, sd3)
	) >>=
	parse synelChromaLocInfoPresentFlag >>= \(bt4, sd4) ->
	(let
		hasCLInfo = sdScalar sd4 synelChromaLocInfoPresentFlag /= 0
	in
		if hasCLInfo then
			Just (bt4, sd4) >>=
			parse synelChromaSampleLocTypeTopField >>=
			parse synelChromaSampleLocTypeBottomField
		else
			Just (bt4, sd4)
	) >>=
	parse synelTimingInfoPresentFlag >>= \(bt5, sd5) ->
	(let
		hasTimingInfo = sdScalar sd5 synelTimingInfoPresentFlag /= 0
	in
		if hasTimingInfo then
			Just (bt5, sd5) >>=
			parse synelNumUnitsInTick >>=
			parse synelTimeScale >>=
			parse synelFixedFrameRateFlag
		else
			Just (bt5, sd5)
	) >>=
	parse synelNalHrdParametersPresentFlag >>= \(bt6, sd6) ->
	(let
		hasNalHrdParams = sdScalar sd6 synelNalHrdParametersPresentFlag /= 0
	in
		if hasNalHrdParams then
			Just (bt6, sd6) >>=
			-- TODO: what if both VCL and NAL HRD parameters are present?
			parseHrdParameters
		else
			Just (bt6, sd6)
	) >>=
	parse synelVclHrdParametersPresentFlag >>= \(bt7, sd7) ->
	(let
		hasVclHrdParams = sdScalar sd7 synelVclHrdParametersPresentFlag /= 0
	in
		if hasVclHrdParams then
			Just (bt7, sd7) >>=
			-- TODO: what if both VCL and NAL HRD parameters are present?
			parseHrdParameters
		else
			Just (bt7, sd7)
	) >>= \(bt8, sd8) ->
	(let
		hasNalHrdParams = sdScalar sd8 synelNalHrdParametersPresentFlag /= 0
		hasVclHrdParams = sdScalar sd8 synelVclHrdParametersPresentFlag /= 0
	in
		if hasNalHrdParams || hasVclHrdParams then
			Just (bt8, sd8) >>=
			parse synelLowDelayHrdFlag
		else
			Just (bt8, sd8)
	) >>=
	parse synelPicStructPresentFlag >>=
	parse synelBitstreamRestrictionFlag >>= \(bt9, sd9) ->
	(let
		hasBSRestriction = sdScalar sd9 synelBitstreamRestrictionFlag /= 0
	in
		if hasBSRestriction then
			Just (bt9, sd9) >>=
			parse synelMotionVectorsOverPicBoundariesFlag >>=
			parse synelMaxBytesPerPicDenom >>=
			parse synelMaxBitsPerMbDenom >>=
			parse synelLog2MaxMvLengthHorizontal >>=
			parse synelLog2MaxMvLengthVertical >>=
			parse synelMaxNumReorderFrames >>=
			parse synelMaxDecFrameBuffering
		else
			Just (bt9, sd9)
	)



------------------------------------------------------------------------------
-- HRD Parameters
------------------------------------------------------------------------------

data HrdParameters =
	HrdParameters {
		-- TODO
	} deriving (Eq, Show)

emptyHrd :: HrdParameters
emptyHrd = HrdParameters
	-- TODO

hrdFromDictionary :: SynelDictionary -> Maybe HrdParameters
hrdFromDictionary sd = Just emptyHrd
	-- TODO



-- spec E.2.2
synelCpbCntMinus1 = mkSynelV "cpb_cnt_minus1" SynelTypeUEv (<=31)
synelBitRateScale = mkSynel "bit_rate_scale" (SynelTypeUn 4)
synelCpbSizeScale = mkSynel "cpb_size_scale" (SynelTypeUn 4)
synelBitRateValueMinus1 = mkSynelV "bit_rate_value_minus1" SynelTypeUEv (<=0xfffffffe)
synelCpbSizeValueMinus1 = mkSynelV "cpb_size_value_minus1" SynelTypeUEv (<=0xfffffffe)
synelCbrFlag = mkSynel "cbr_flag" (SynelTypeUn 1)
synelInitialCpbRemovalDelayLengthMinus1 = mkSynel "initial_cpb_removal_delay_length_minus1" (SynelTypeUn 5)
synelCpbRemovalDelayLengthMinus1 = mkSynel "cpb_removal_delay_length_minus1" (SynelTypeUn 5)
synelDpbOutputDelayLengthMinus1 = mkSynel "dpb_output_delay_length_minus1" (SynelTypeUn 5)
synelTimeOffsetLength = mkSynel "time_offset_length" (SynelTypeUn 5)


-- spec E.1.2
parseHrdParameters :: (BitstreamBE, SynelDictionary) -> Maybe (BitstreamBE, SynelDictionary)
parseHrdParameters _ | trace "parseHrdParameters" False = undefined
parseHrdParameters (bt, sd) =
	Just (bt, sd) >>=
	parse synelCpbCntMinus1 >>=
	parse synelBitRateScale >>=
	parse synelCpbSizeScale >>= \(bt1, sd1) ->
	(let
		cpbCntMinus1 = sdScalar sd1 synelCpbCntMinus1
	in
		Just (bt1, sd1) >>=
		parseForEach [0..cpbCntMinus1] (\_ (bt11, sd11) ->
			Just (bt11, sd11) >>=
			parseA synelBitRateValueMinus1 >>=
			parseA synelCpbSizeValueMinus1 >>=
			parseA synelCbrFlag
		)
	) >>=
	parse synelInitialCpbRemovalDelayLengthMinus1 >>=
	parse synelCpbRemovalDelayLengthMinus1 >>=
	parse synelDpbOutputDelayLengthMinus1 >>=
	parse synelTimeOffsetLength

