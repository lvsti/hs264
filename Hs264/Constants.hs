-- Hs264.Constants

module Hs264.Constants where

kBlock4x4To8x8 = [0, 0, 1, 1, 
                  0, 0, 1, 1,
                  2, 2, 3, 3,
                  2, 2, 3, 3]

data CUNamedBlock = KCU4x4BlockLumaDC | -- 0
					KCU4x4BlockLuma0 | -- 1
					KCU4x4BlockLuma1 |
					KCU4x4BlockLuma2 |
					KCU4x4BlockLuma3 |
					KCU4x4BlockLuma4 |
					KCU4x4BlockLuma5 |
					KCU4x4BlockLuma6 |
					KCU4x4BlockLuma7 |
					KCU4x4BlockLuma8 |
					KCU4x4BlockLuma9 |
					KCU4x4BlockLuma10 |
					KCU4x4BlockLuma11 |
					KCU4x4BlockLuma12 |
					KCU4x4BlockLuma13 |
					KCU4x4BlockLuma14 |
					KCU4x4BlockLuma15 |
					KCU2x2BlockBChromaDC | -- 17
					KCU2x2BlockRChromaDC | -- 18
					KCU4x4BlockBChroma0 | -- 19
					KCU4x4BlockBChroma1 | 
					KCU4x4BlockBChroma2 |
					KCU4x4BlockBChroma3 | -- 22
					KCU4x4BlockRChroma0 | -- 23
					KCU4x4BlockRChroma1 |
					KCU4x4BlockRChroma2 |
					KCU4x4BlockRChroma3
					deriving (Eq, Ord, Show, Read, Enum, Bounded)

cuLumaBlock :: Int -> CUNamedBlock
cuLumaBlock idx = toEnum $ fromEnum KCU4x4BlockLuma0 + idx

cuBChromaBlock :: Int -> CUNamedBlock
cuBChromaBlock idx = toEnum $ fromEnum KCU4x4BlockBChroma0 + idx

cuRChromaBlock :: Int -> CUNamedBlock
cuRChromaBlock idx = toEnum $ fromEnum KCU4x4BlockRChroma0 + idx





data PictureType = KPictureTypeI |
				   KPictureTypeIP |
				   KPictureTypeIPB |
				   KPictureTypeSI |
				   KPictureTypeSISP |
				   KPictureTypeISI |
				   KPictureTypeISIPSP |
				   KPictureTypeISIPSPB
				   deriving (Eq, Ord, Show, Read, Enum, Bounded)
				   


data SliceType = KSliceTypeP |
				 KSliceTypeB |
				 KSliceTypeI |
				 KSliceTypeSP |
				 KSliceTypeSI |
				 KSliceTypePStrict |
				 KSliceTypeBStrict |
				 KSliceTypeIStrict |
				 KSliceTypeSPStrict |
				 KSliceTypeSIStrict
				 deriving (Eq, Ord, Show, Read, Enum, Bounded)













