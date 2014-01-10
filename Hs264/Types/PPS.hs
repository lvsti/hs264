-- Hs264.Types.PPS

module Hs264.Types.PPS where

import Hs264.Types


-------------------------------------------------------------------------------
-- Slice group mapping descriptor
-------------------------------------------------------------------------------

data SliceGroupMapParameters =
 	-- type = 0
 	SgmInterleaved {
		sgmRunLengthMinus1 :: [Int]
	} |
	-- type = 1
	SgmDispersed |
	-- type = 2
 	SgmForegroundLeftover {
		sgmTopLeft :: [Int],
		sgmBottomRight :: [Int]
	} |
	-- type = 3,4,5
	SgmChanging {
		sgmSliceGroupChangeDirectionFlag :: Bool,
		sgmSliceGroupChangeRate :: Int
	} |
	-- type = 6
	SgmExplicit {
		sgmPicSizeInMapUnits :: Int,
		sgmSliceGroupId :: [Int]
	}
	deriving (Eq, Show)



-------------------------------------------------------------------------------
-- Picture Parameter Set data type
-------------------------------------------------------------------------------

data PictureParameterSet =
	PictureParameterSet {
		-- core parameters
		ppsPicParameterSetId :: Int,
		ppsSeqParameterSetId :: Int,
		ppsEntropyCodingModeFlag :: Bool,
		ppsBottomFieldPicOrderInFramePresentFlag :: Bool,
		ppsNumRefIdxL0DefaultActive :: Int,
		ppsNumRefIdxL1DefaultActive :: Int,
		ppsWeightedPredFlag :: Bool,
		ppsWeightedBipredIdc :: Int,
		ppsPicInitQp :: Int,
		ppsPicInitQs :: Int,
		ppsChromaQpIndexOffset :: Int,
		ppsDeblockingFilterControlPresentFlag :: Bool,
		ppsConstrainedIntraPredFlag :: Bool,
		ppsRedundantPicCntPresentFlag :: Bool,
		-- slice groups
		ppsNumSliceGroups :: Int,
		ppsSliceGroupMapType :: Int,
		ppsSliceGroupMapParameters :: Maybe SliceGroupMapParameters,
		-- optional parameters
		ppsTransform8x8ModeFlag :: Bool,
		ppsPicScalingMatrixPresentFlag :: Bool,
		ppsScalingLists :: [[Arithmetic]],
		ppsSecondChromaQpIndexOffset :: Int
	} deriving (Eq, Show)


