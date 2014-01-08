-- Hs264.Types.Profile

module Hs264.Types.Profile where


-------------------------------------------------------------------------------
-- Decoder profiles and levels (spec Annex 2)
-------------------------------------------------------------------------------

data H264Profile =
	Profile {
		prfProfileIdc :: Int,
		prfConformanceTest :: Int -> [Bool] -> Int -> H264Level -> Bool
	}

instance Show H264Profile where
	show Profile { prfProfileIdc = pidc } = "Profile(" ++ show pidc ++ ")"


data H264Level =
	Level1 |
	Level1b |
	Level1_1 |
	Level1_2 |
	Level1_3 |
	Level2 |
	Level2_1 |
	Level2_2 |
	Level3 |
	Level3_1 |
	Level3_2 |
	Level4 |
	Level4_1 |
	Level4_2 |
	Level5 |
	Level5_1 |
	Level5_2 |
	LevelAny
	deriving (Eq, Ord, Show, Read, Enum, Bounded)

levelFromIdc :: Int -> [Bool] -> H264Level
levelFromIdc lidc csFlags = level
	 where
		 level = if lidc == 11 && csFlags !! 3 then
			 		 Level1b
				 else
					 (levels !! (lidc `div` 10)) !! (lidc `mod` 10)
		 levels = [[Level1, Level1_1, Level1_2, Level1_3],
		 		   [Level2, Level2_1, Level2_2],
				   [Level3,	Level3_1, Level3_2],
				   [Level4, Level4_1, Level4_2],
				   [Level5, Level5_1, Level5_2]]


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

