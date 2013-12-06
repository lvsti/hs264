-- Hs264.Transform

module Hs264.Transform where

import Data.List
import Data.Bits

import Hs264.Types
import Hs264.Functions
import Hs264.Block





inverse4x4Residual :: Int -> Arithmetic4x4 -> Arithmetic4x4
inverse4x4Residual qp = inverseHT4x4Residual . (rescale4x4Residual qp)

inverse4x4LumaDC :: Int -> Arithmetic4x4 -> Arithmetic4x4
inverse4x4LumaDC qp = (rescale4x4LumaDC qp) . inverseHT4x4LumaDC



kChromaQP30Map = [29,30,31,32,32,33,34,34,35,35, 36,36,37,37,37,38,38,38,39,39, 39,39]

chromaQpFromLumaWithOffset :: Int -> Int -> Int
chromaQpFromLumaWithOffset qpl ofs
	| qpc < 30 = qpl
	| otherwise = kChromaQP30Map !! (qpl-30)
	where
		qpc = clip3 0 51 (qpl + ofs)



kInverseHTResidualDoubled = [[2,2,2,1],
							 [2,1,-2,-2],
							 [2,-1,-2,2],
							 [2,-2,2,-1]] :: [[Arithmetic]]

inverseHTResidualDoubled1D :: [Arithmetic] -> [Arithmetic]
inverseHTResidualDoubled1D xs = map (sum . zipWith (*) xs) kInverseHTResidualDoubled
-- inverseFastHadamard1D xs = [sum02 + sum13, diff02 + diff13, diff02 - diff13, sum02 - sum13]
-- 	where
-- 		sum02 = xs !! 0 + xs !! 2
-- 		sum13 = xs !! 1 + (xs !! 3) `div` 2
-- 		diff02 = xs !! 0 - xs !! 2
-- 		diff13 = (xs !! 1) `div` 2 - xs !! 3

inverseHT4x4Residual :: Arithmetic4x4 -> Arithmetic4x4
inverseHT4x4Residual blk = fromRaster $ map rescale $ concat rows
	where
		htColumns = toColumns blk
		columns = map inverseHTResidualDoubled1D htColumns
		htRows = transpose columns
		rows = map inverseHTResidualDoubled1D htRows
		rescale = \x -> (x+128) `shiftR` 8




kInverseHTLumaDC = [[1,1,1,1],
					[1,1,-1,-1],
					[1,-1,-1,1],
					[1,-1,1,-1]] :: [[Arithmetic]]

inverseHTLumaDC1D :: [Arithmetic] -> [Arithmetic]
inverseHTLumaDC1D xs = map (sum . zipWith (*) xs) kInverseHTLumaDC

inverseHT4x4LumaDC :: Arithmetic4x4 -> Arithmetic4x4
inverseHT4x4LumaDC blk = fromRows rows
	where
		htColumns = toColumns blk
		columns = map inverseHTLumaDC1D htColumns
		htRows = transpose columns
		rows = map inverseHTLumaDC1D htRows



kInverseHTChromaDC = [[1,1],
					  [1,-1]] :: [[Arithmetic]]

inverseHTChromaDC1D :: [Arithmetic] -> [Arithmetic]
inverseHTChromaDC1D xs = map (sum . zipWith (*) xs) kInverseHTChromaDC

inverseHT2x2ChromaDC :: Arithmetic2x2 -> Arithmetic2x2
inverseHT2x2ChromaDC blk = fromRows rows
	where
		htColumns = toColumns blk
		columns = map inverseHTChromaDC1D htColumns
		htRows = transpose columns
		rows = map inverseHTChromaDC1D htRows



-- v_base: inverse quantizer values for QP = 0..5
-- V[i] ~= Qstep(QP) * PF[i] * 64
-- a2, b2, ab
kVBase4x4 = [[10, 16, 13],
		  	 [11, 18, 14],
		  	 [13, 20, 16],
		  	 [14, 23, 18],
		  	 [16, 25, 20],
		  	 [18, 29, 23]] :: [[Arithmetic]]

inverseHT4x4Rescale :: Int -> [Arithmetic]
inverseHT4x4Rescale qp = [a2, ab, a2, ab,
						  ab, b2, ab, b2,
						  a2, ab, a2, ab,
						  ab, b2, ab, b2]
	where
		[a2, b2, ab] = kVBase4x4 !! (qp `mod` 6)


rescale4x4Residual :: Int -> Arithmetic4x4 -> Arithmetic4x4
rescale4x4Residual qp blk = fromRaster $ zipWith rescaleResidual vs $ toRaster blk
	where
		vs = inverseHT4x4Rescale qp
		qpd6 = qp `div` 6
		rescaleResidual = if qp >= 24 then rescaleResidual4x4HighQP else rescaleResidual4x4LowQP
				
		rescaleResidual4x4HighQP :: Arithmetic -> Arithmetic -> Arithmetic
		rescaleResidual4x4HighQP v x = (v*x) `shiftL` qpShift
			where
				qpShift = qpd6 - 4

		rescaleResidual4x4LowQP :: Arithmetic -> Arithmetic -> Arithmetic
		rescaleResidual4x4LowQP v x = (v*x + fq) `shiftR` qpShift
			where
				fq = 1 `shiftL` (3-qpd6)
				qpShift = 4 - qpd6


rescale4x4LumaDC :: Int -> Arithmetic4x4 -> Arithmetic4x4
rescale4x4LumaDC qp blk = fmap rescaleLumaDC blk
	where
		vs = inverseHT4x4Rescale qp
		v0 = vs !! 0
		qpd6 = qp `div` 6
		rescaleLumaDC = if qp >= 36 then rescaleLumaDCHighQP else rescaleLumaDCLowQP
		
		rescaleLumaDCHighQP :: Arithmetic -> Arithmetic
		rescaleLumaDCHighQP x = (v0*x) `shiftL` qpShift
			where
				qpShift = qpd6 - 6
		
		rescaleLumaDCLowQP :: Arithmetic -> Arithmetic
		rescaleLumaDCLowQP x = (v0*x + fq) `shiftR` qpShift
			where
				fq = 1 `shiftL` (5-qpd6)
				qpShift = 6 - qpd6


rescale2x2ChromaDC :: Int -> Arithmetic2x2 -> Arithmetic2x2
rescale2x2ChromaDC qp blk = fmap rescaleChromaDC420 blk
	where
		vs = inverseHT4x4Rescale qp
		v0 = vs !! 0
		qpd6 = qp `div` 6
		
		rescaleChromaDC420 :: Arithmetic -> Arithmetic
		rescaleChromaDC420 x = ((v0*x) `shiftL` qpd6) `shiftR` 5
		

rescale4x2ChromaDC :: Int -> Arithmetic4x2 -> Arithmetic4x2
rescale4x2ChromaDC qp blk = fmap rescaleChromaDC blk
	where
		vs = inverseHT4x4Rescale qp
		v0 = vs !! 0
		qpd6 = qp `div` 6
		rescaleChromaDC = if qp+3 >= 36 then rescaleChromaDC422HighQP else rescaleChromaDC422LowQP

		rescaleChromaDC422HighQP :: Arithmetic -> Arithmetic
		rescaleChromaDC422HighQP x = (v0*x) `shiftL` qpShift
			where
				qpShift = qpd6 - 6
		
		rescaleChromaDC422LowQP :: Arithmetic -> Arithmetic
		rescaleChromaDC422LowQP x = (v0*x + fq) `shiftR` qpShift
			where
				fq = 1 `shiftL` (5-qpd6)
				qpShift = 6 - qpd6











