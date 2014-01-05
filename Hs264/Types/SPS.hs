-- Hs264.Types.SPS

module Hs264.Types.SPS where

import Hs264.Types.VUI



-------------------------------------------------------------------------------
-- Sequence Parameter Set data type
-------------------------------------------------------------------------------

data SequenceParameterSet =
	SequenceParameterSet {
		-- profiles and levels
		spsProfileIdc :: Int,
		spsConstraintSetFlags :: [Bool],
		spsLevelIdc :: Int,
		-- color format
		spsChromaFormatIdc :: Int,
		spsSeparateColourPlaneFlag :: Bool,
		spsChromaArrayType :: Int,
		spsBitDepthY :: Int,
		spsBitDepthC :: Int,
		spsQpBdOffsetY :: Int,
		spsQpBdOffsetC :: Int,
		-- coefficient transformation
		spsQpPrimeYZeroTransformBypassFlag :: Bool,
		spsSeqScalingMatrixPresentFlag :: Bool,
		spsScalingLists :: [[Int]],
		-- frame ordering
		spsMaxFrameNum :: Int,
		spsPicOrderCntType :: Int,
		spsMaxPicOrderCntLsb :: Int,
		spsDeltaPicOrderAlwaysZeroFlag :: Bool,
		spsOffsetForNonRefPic :: Int,
		spsOffsetForTopToBottomField :: Int,
		spsNumRefFramesInPicOrderCntCycle :: Int,
		spsRefFrameOffsets :: [Int],
		spsMaxNumRefFrames :: Int,
		spsGapsInFrameNumValueAllowedFlag :: Bool,
		-- frame dimensions
		spsPicWidthInMbs :: Int,
		spsPicHeightInMapUnits :: Int,
		spsFrameCroppingFlag :: Bool,
		spsFrameCropLeftOffset :: Int,
		spsFrameCropRightOffset :: Int,
		spsFrameCropTopOffset :: Int,
		spsFrameCropBottomOffset :: Int,
		-- miscellaneous
		spsSeqParameterSetId :: Int,
		spsFrameMbsOnlyFlag :: Bool,
		spsMbAdaptiveFrameFieldFlag :: Bool,
		spsDirect8x8InferenceFlag :: Bool,
		-- video usability info
		spsVuiParametersPresentFlag :: Bool,
		spsVuiParameters :: Maybe VuiParameters
	} deriving (Eq, Show)


