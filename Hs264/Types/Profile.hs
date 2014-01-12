-- Hs264.Types.Profile

module Hs264.Types.Profile where


-------------------------------------------------------------------------------
-- Decoder profiles (spec Annex 2)
-------------------------------------------------------------------------------

data H264Profile =
	Profile {
		prfProfileIdc :: Int,
		prfConformanceTest :: Int -> [Bool] -> Int -> H264Level -> Bool
	}

instance Show H264Profile where
	show Profile { prfProfileIdc = pidc } = "Profile(" ++ show pidc ++ ")"


-------------------------------------------------------------------------------
-- Predefined profiles as in spec A.2
-------------------------------------------------------------------------------

kProfileBaseline =
	Profile {
		prfProfileIdc = 66,
		prfConformanceTest = \pidc csFlags lidc lv ->
			(pidc == 66 || csFlags !! 0) && levelFromIdc lidc csFlags <= lv
	}

kProfileConstrainedBaseline =
	Profile {
		prfProfileIdc = 66,
		prfConformanceTest = \pidc csFlags lidc lv ->
			(pidc == 66 || csFlags !! 0) && csFlags !! 1 && levelFromIdc lidc csFlags <= lv
	}

kProfileMain =
	Profile {
		prfProfileIdc = 77,
		prfConformanceTest = \pidc csFlags lidc lv ->
			(pidc == 77 || csFlags !! 1) && levelFromIdc lidc csFlags <= lv
	}
	
kProfileExtended =
	Profile {
		prfProfileIdc = 88,
		prfConformanceTest = \pidc csFlags lidc lv ->
			(pidc == 88 || csFlags !! 2) && levelFromIdc lidc (repeat False) <= lv ||
			(pidc == 66 || csFlags !! 0) && levelFromIdc lidc csFlags <= lv
	}

kProfileHigh =
	Profile {
		prfProfileIdc = 100,
		prfConformanceTest = \pidc csFlags lidc lv ->
			(pidc == 77 || csFlags !! 1) && levelFromIdc lidc csFlags <= lv ||
			pidc == 100 && levelFromIdc lidc (repeat False) <= lv
	}

kProfileProgressiveHigh =
	Profile {
		prfProfileIdc = 100,
		prfConformanceTest = \pidc csFlags lidc lv -> 
			((pidc == 66 || csFlags !! 0) && csFlags !! 1 ||
			 pidc == 77 && csFlags !! 0 ||
			 pidc == 77 && csFlags !! 4 ||
			 pidc == 88 && csFlags !! 1 && csFlags !! 4) && levelFromIdc lidc csFlags <= lv ||
			(pidc == 100 && csFlags !! 4 && levelFromIdc lidc (repeat False) <= lv)
	}

kProfileConstrainedHigh =
	Profile {
		prfProfileIdc = 100,
		prfConformanceTest = \pidc csFlags lidc lv -> 
			((pidc == 66 || csFlags !! 0) && csFlags !! 1 ||
			 pidc == 77 && csFlags !! 0 ||
			 pidc == 88 && csFlags !! 1 && csFlags !! 4 && csFlags !! 5) && levelFromIdc lidc csFlags <= lv ||
			(pidc == 77 && csFlags !! 4 && csFlags !! 5 ||
			 pidc == 100 && csFlags !! 4) && levelFromIdc lidc (repeat False) <= lv
	}

kProfileHigh10 =
	Profile {
		prfProfileIdc = 110,
		prfConformanceTest = \pidc csFlags lidc lv ->
			(pidc == 77 || csFlags !! 1) && levelFromIdc lidc csFlags <= lv ||
			(pidc `elem` [100,110]) && levelFromIdc lidc (repeat False) <= lv
	}

kProfileHigh422 =
	Profile {
		prfProfileIdc = 122,
		prfConformanceTest = \pidc csFlags lidc lv ->
			(pidc == 77 || csFlags !! 1) && levelFromIdc lidc csFlags <= lv ||
			(pidc `elem` [100,110,122]) && levelFromIdc lidc (repeat False) <= lv
	}

kProfileHigh444Predictive =
	Profile {
		prfProfileIdc = 244,
		prfConformanceTest = \pidc csFlags lidc lv ->
			(pidc == 77 || csFlags !! 1) && levelFromIdc lidc csFlags <= lv ||
			(pidc `elem` [44,100,110,122,244]) && levelFromIdc lidc (repeat False) <= lv
	}

kProfileHigh10Intra =
	Profile {
		prfProfileIdc = 110,
		prfConformanceTest = \pidc csFlags lidc lv ->
			(pidc `elem` [100,110]) && csFlags !! 3 && levelFromIdc lidc (repeat False) <= lv
	}

kProfileHigh422Intra =
	Profile {
		prfProfileIdc = 122,
		prfConformanceTest = \pidc csFlags lidc lv ->
			(pidc `elem` [100,110,122]) && csFlags !! 3 && levelFromIdc lidc (repeat False) <= lv
	}

kProfileHigh444Intra =
	Profile {
		prfProfileIdc = 244,
		prfConformanceTest = \pidc csFlags lidc lv ->
			(pidc `elem` [44,100,110,122,244]) && csFlags !! 3 && levelFromIdc lidc (repeat False) <= lv
	}

kProfileCavlc444Intra =
	Profile {
		prfProfileIdc = 44,
		prfConformanceTest = \pidc csFlags lidc lv ->
			pidc == 44 && levelFromIdc lidc (repeat False) <= lv
	}



-------------------------------------------------------------------------------
-- Decoder levels (spec Annex 2)
-------------------------------------------------------------------------------

data H264Level =
	Level {
		lvlLevelIdc :: Int,
		lvlIntermediateFlag :: Bool,
		-- Table A-1
		lvlMaxMbsPerSecond :: Int,
		lvlMaxFrameSize :: Int,
		lvlMaxDpbMbs :: Int,
		lvlMaxBitRate :: Int,
		lvlMaxCpbSize :: Int,
		lvlMaxVerticalMvRangeQpx :: (Int, Int),
		lvlMinCompressionRatio :: Int,
		lvlMaxMvsPer2Mbs :: Maybe Int,
		lvlMaxSubMbRectSize :: Maybe Int,
		lvlSliceRate :: Maybe Int,
		lvlMinLumaBipredSize :: Maybe Int,
		lvlDirect8x8InferenceFlag :: Maybe Bool,
		lvlFrameMbsOnlyFlag :: Maybe Bool
	} deriving (Eq)


instance Ord H264Level where
	(<=) l1 l2 = lvlLevelIdc l1 < lvlLevelIdc l2 || 
				 lvlLevelIdc l1 == lvlLevelIdc l2 && not (lvlIntermediateFlag l1)

instance Show H264Level where
	show Level { lvlLevelIdc = lidc, lvlIntermediateFlag = imf } =
		"Level" ++ (if imf then "1b" else show lmaj ++ (if lmin /= 0 then "." ++ show lmin else ""))
		where
			lmaj = lidc `div` 10
			lmin = lidc `mod` 10


levelFromIdc :: Int -> [Bool] -> H264Level
levelFromIdc lidc csFlags = level
	 where
		 level = if lidc == 11 && csFlags !! 3 then
			 		 kLevel1b
				 else
					 (levels !! (lidc `div` 10)) !! (lidc `mod` 10)
		 levels = [[kLevel1, kLevel1_1, kLevel1_2, kLevel1_3],
		 		   [kLevel2, kLevel2_1, kLevel2_2],
				   [kLevel3, kLevel3_1, kLevel3_2],
				   [kLevel4, kLevel4_1, kLevel4_2],
				   [kLevel5, kLevel5_1, kLevel5_2]]


-------------------------------------------------------------------------------
-- Predefined levels as in spec A.2 Table A-1
-------------------------------------------------------------------------------

kLevel1 =
	Level {
		lvlLevelIdc = 10,
		lvlIntermediateFlag = False,
		lvlMaxMbsPerSecond = 1485,
		lvlMaxFrameSize = 99,
		lvlMaxDpbMbs = 396,
		lvlMaxBitRate = 64,
		lvlMaxCpbSize = 175,
		lvlMaxVerticalMvRangeQpx = (-256, 255),
		lvlMinCompressionRatio = 2,
		lvlMaxMvsPer2Mbs = Nothing,
		lvlMaxSubMbRectSize = Just 576,
		lvlSliceRate = Nothing,
		lvlMinLumaBipredSize = Nothing,
		lvlDirect8x8InferenceFlag = Nothing,
		lvlFrameMbsOnlyFlag = Just True
	}

kLevel1b =
	Level {
		lvlLevelIdc = 11,
		lvlIntermediateFlag = True,
		lvlMaxMbsPerSecond = 1485,
		lvlMaxFrameSize = 99,
		lvlMaxDpbMbs = 396,
		lvlMaxBitRate = 128,
		lvlMaxCpbSize = 350,
		lvlMaxVerticalMvRangeQpx = (-256, 255),
		lvlMinCompressionRatio = 2,
		lvlMaxMvsPer2Mbs = Nothing,
		lvlMaxSubMbRectSize = Just 576,
		lvlSliceRate = Nothing,
		lvlMinLumaBipredSize = Nothing,
		lvlDirect8x8InferenceFlag = Nothing,
		lvlFrameMbsOnlyFlag = Just True
	}

kLevel1_1 =
	Level {
		lvlLevelIdc = 11,
		lvlIntermediateFlag = False,
		lvlMaxMbsPerSecond = 3000,
		lvlMaxFrameSize = 396,
		lvlMaxDpbMbs = 900,
		lvlMaxBitRate = 192,
		lvlMaxCpbSize = 500,
		lvlMaxVerticalMvRangeQpx = (-512, 511),
		lvlMinCompressionRatio = 2,
		lvlMaxMvsPer2Mbs = Nothing,
		lvlMaxSubMbRectSize = Just 576,
		lvlSliceRate = Nothing,
		lvlMinLumaBipredSize = Nothing,
		lvlDirect8x8InferenceFlag = Nothing,
		lvlFrameMbsOnlyFlag = Just True
	}

kLevel1_2 =
	Level {
		lvlLevelIdc = 12,
		lvlIntermediateFlag = False,
		lvlMaxMbsPerSecond = 6000,
		lvlMaxFrameSize = 396,
		lvlMaxDpbMbs = 2376,
		lvlMaxBitRate = 384,
		lvlMaxCpbSize = 1000,
		lvlMaxVerticalMvRangeQpx = (-512, 511),
		lvlMinCompressionRatio = 2,
		lvlMaxMvsPer2Mbs = Nothing,
		lvlMaxSubMbRectSize = Just 576,
		lvlSliceRate = Nothing,
		lvlMinLumaBipredSize = Nothing,
		lvlDirect8x8InferenceFlag = Nothing,
		lvlFrameMbsOnlyFlag = Just True
	}

kLevel1_3 =
	Level {
		lvlLevelIdc = 13,
		lvlIntermediateFlag = False,
		lvlMaxMbsPerSecond = 11880,
		lvlMaxFrameSize = 396,
		lvlMaxDpbMbs = 2376,
		lvlMaxBitRate = 768,
		lvlMaxCpbSize = 2000,
		lvlMaxVerticalMvRangeQpx = (-512, 511),
		lvlMinCompressionRatio = 2,
		lvlMaxMvsPer2Mbs = Nothing,
		lvlMaxSubMbRectSize = Just 576,
		lvlSliceRate = Nothing,
		lvlMinLumaBipredSize = Nothing,
		lvlDirect8x8InferenceFlag = Nothing,
		lvlFrameMbsOnlyFlag = Just True
	}

kLevel2 =
	Level {
		lvlLevelIdc = 20,
		lvlIntermediateFlag = False,
		lvlMaxMbsPerSecond = 11880,
		lvlMaxFrameSize = 396,
		lvlMaxDpbMbs = 2376,
		lvlMaxBitRate = 2000,
		lvlMaxCpbSize = 2000,
		lvlMaxVerticalMvRangeQpx = (-512, 511),
		lvlMinCompressionRatio = 2,
		lvlMaxMvsPer2Mbs = Nothing,
		lvlMaxSubMbRectSize = Just 576,
		lvlSliceRate = Nothing,
		lvlMinLumaBipredSize = Nothing,
		lvlDirect8x8InferenceFlag = Nothing,
		lvlFrameMbsOnlyFlag = Just True
	}

kLevel2_1 =
	Level {
		lvlLevelIdc = 21,
		lvlIntermediateFlag = False,
		lvlMaxMbsPerSecond = 19800,
		lvlMaxFrameSize = 792,
		lvlMaxDpbMbs = 4752,
		lvlMaxBitRate = 4000,
		lvlMaxCpbSize = 4000,
		lvlMaxVerticalMvRangeQpx = (-1024, 1023),
		lvlMinCompressionRatio = 2,
		lvlMaxMvsPer2Mbs = Nothing,
		lvlMaxSubMbRectSize = Just 576,
		lvlSliceRate = Nothing,
		lvlMinLumaBipredSize = Nothing,
		lvlDirect8x8InferenceFlag = Nothing,
		lvlFrameMbsOnlyFlag = Nothing
	}

kLevel2_2 =
	Level {
		lvlLevelIdc = 22,
		lvlIntermediateFlag = False,
		lvlMaxMbsPerSecond = 20250,
		lvlMaxFrameSize = 1620,
		lvlMaxDpbMbs = 8100,
		lvlMaxBitRate = 4000,
		lvlMaxCpbSize = 4000,
		lvlMaxVerticalMvRangeQpx = (-1024, 1023),
		lvlMinCompressionRatio = 2,
		lvlMaxMvsPer2Mbs = Nothing,
		lvlMaxSubMbRectSize = Just 576,
		lvlSliceRate = Nothing,
		lvlMinLumaBipredSize = Nothing,
		lvlDirect8x8InferenceFlag = Nothing,
		lvlFrameMbsOnlyFlag = Nothing
	}

kLevel3 =
	Level {
		lvlLevelIdc = 30,
		lvlIntermediateFlag = False,
		lvlMaxMbsPerSecond = 40500,
		lvlMaxFrameSize = 1620,
		lvlMaxDpbMbs = 8100,
		lvlMaxBitRate = 10000,
		lvlMaxCpbSize = 10000,
		lvlMaxVerticalMvRangeQpx = (-1024, 1023),
		lvlMinCompressionRatio = 2,
		lvlMaxMvsPer2Mbs = Just 32,
		lvlMaxSubMbRectSize = Just 576,
		lvlSliceRate = Just 22,
		lvlMinLumaBipredSize = Nothing,
		lvlDirect8x8InferenceFlag = Just True,
		lvlFrameMbsOnlyFlag = Nothing
	}

kLevel3_1 =
	Level {
		lvlLevelIdc = 31,
		lvlIntermediateFlag = False,
		lvlMaxMbsPerSecond = 108000,
		lvlMaxFrameSize = 3600,
		lvlMaxDpbMbs = 18000,
		lvlMaxBitRate = 14000,
		lvlMaxCpbSize = 14000,
		lvlMaxVerticalMvRangeQpx = (-2048, 2047),
		lvlMinCompressionRatio = 4,
		lvlMaxMvsPer2Mbs = Just 16,
		lvlMaxSubMbRectSize = Nothing,
		lvlSliceRate = Just 60,
		lvlMinLumaBipredSize = Just 8,
		lvlDirect8x8InferenceFlag = Just True,
		lvlFrameMbsOnlyFlag = Nothing
	}

kLevel3_2 =
	Level {
		lvlLevelIdc = 32,
		lvlIntermediateFlag = False,
		lvlMaxMbsPerSecond = 216000,
		lvlMaxFrameSize = 5120,
		lvlMaxDpbMbs = 20480,
		lvlMaxBitRate = 20000,
		lvlMaxCpbSize = 20000,
		lvlMaxVerticalMvRangeQpx = (-2048, 2047),
		lvlMinCompressionRatio = 4,
		lvlMaxMvsPer2Mbs = Just 16,
		lvlMaxSubMbRectSize = Nothing,
		lvlSliceRate = Just 60,
		lvlMinLumaBipredSize = Just 8,
		lvlDirect8x8InferenceFlag = Just True,
		lvlFrameMbsOnlyFlag = Nothing
	}

kLevel4 =
	Level {
		lvlLevelIdc = 40,
		lvlIntermediateFlag = False,
		lvlMaxMbsPerSecond = 245760,
		lvlMaxFrameSize = 8192,
		lvlMaxDpbMbs = 32768,
		lvlMaxBitRate = 20000,
		lvlMaxCpbSize = 25000,
		lvlMaxVerticalMvRangeQpx = (-2048, 2047),
		lvlMinCompressionRatio = 4,
		lvlMaxMvsPer2Mbs = Just 16,
		lvlMaxSubMbRectSize = Nothing,
		lvlSliceRate = Just 60,
		lvlMinLumaBipredSize = Just 8,
		lvlDirect8x8InferenceFlag = Just True,
		lvlFrameMbsOnlyFlag = Nothing
	}

kLevel4_1 =
	Level {
		lvlLevelIdc = 41,
		lvlIntermediateFlag = False,
		lvlMaxMbsPerSecond = 245760,
		lvlMaxFrameSize = 8192,
		lvlMaxDpbMbs = 32768,
		lvlMaxBitRate = 50000,
		lvlMaxCpbSize = 62500,
		lvlMaxVerticalMvRangeQpx = (-2048, 2047),
		lvlMinCompressionRatio = 2,
		lvlMaxMvsPer2Mbs = Just 16,
		lvlMaxSubMbRectSize = Nothing,
		lvlSliceRate = Just 24,
		lvlMinLumaBipredSize = Just 8,
		lvlDirect8x8InferenceFlag = Just True,
		lvlFrameMbsOnlyFlag = Nothing
	}

kLevel4_2 =
	Level {
		lvlLevelIdc = 42,
		lvlIntermediateFlag = False,
		lvlMaxMbsPerSecond = 522240,
		lvlMaxFrameSize = 8704,
		lvlMaxDpbMbs = 34816,
		lvlMaxBitRate = 50000,
		lvlMaxCpbSize = 62500,
		lvlMaxVerticalMvRangeQpx = (-2048, 2047),
		lvlMinCompressionRatio = 2,
		lvlMaxMvsPer2Mbs = Just 16,
		lvlMaxSubMbRectSize = Nothing,
		lvlSliceRate = Just 24,
		lvlMinLumaBipredSize = Just 8,
		lvlDirect8x8InferenceFlag = Just True,
		lvlFrameMbsOnlyFlag = Just True
	}

kLevel5 =
	Level {
		lvlLevelIdc = 50,
		lvlIntermediateFlag = False,
		lvlMaxMbsPerSecond = 589824,
		lvlMaxFrameSize = 22080,
		lvlMaxDpbMbs = 110400,
		lvlMaxBitRate = 135000,
		lvlMaxCpbSize = 135000,
		lvlMaxVerticalMvRangeQpx = (-2048, 2047),
		lvlMinCompressionRatio = 2,
		lvlMaxMvsPer2Mbs = Just 16,
		lvlMaxSubMbRectSize = Nothing,
		lvlSliceRate = Just 24,
		lvlMinLumaBipredSize = Just 8,
		lvlDirect8x8InferenceFlag = Just True,
		lvlFrameMbsOnlyFlag = Just True
	}

kLevel5_1 =
	Level {
		lvlLevelIdc = 51,
		lvlIntermediateFlag = False,
		lvlMaxMbsPerSecond = 983040,
		lvlMaxFrameSize = 36864,
		lvlMaxDpbMbs = 184320,
		lvlMaxBitRate = 240000,
		lvlMaxCpbSize = 240000,
		lvlMaxVerticalMvRangeQpx = (-2048, 2047),
		lvlMinCompressionRatio = 2,
		lvlMaxMvsPer2Mbs = Just 16,
		lvlMaxSubMbRectSize = Nothing,
		lvlSliceRate = Just 24,
		lvlMinLumaBipredSize = Just 8,
		lvlDirect8x8InferenceFlag = Just True,
		lvlFrameMbsOnlyFlag = Just True
	}

kLevel5_2 =
	Level {
		lvlLevelIdc = 52,
		lvlIntermediateFlag = False,
		lvlMaxMbsPerSecond = 2073600,
		lvlMaxFrameSize = 36864,
		lvlMaxDpbMbs = 184320,
		lvlMaxBitRate = 240000,
		lvlMaxCpbSize = 240000,
		lvlMaxVerticalMvRangeQpx = (-2048, 2047),
		lvlMinCompressionRatio = 2,
		lvlMaxMvsPer2Mbs = Just 16,
		lvlMaxSubMbRectSize = Nothing,
		lvlSliceRate = Just 24,
		lvlMinLumaBipredSize = Just 8,
		lvlDirect8x8InferenceFlag = Just True,
		lvlFrameMbsOnlyFlag = Just True
	}
