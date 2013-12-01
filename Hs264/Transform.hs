-- Hs264.Transform

module Hs264.Transform where

import Data.List
import Data.Bits

import Hs264.Types
import Hs264.Functions
import Hs264.Block


ttb = [5,11,8,10, 9,8,4,12, 1,10,11,4, 19,6,15,7] :: [Arithmetic]
tz = [17,0,-1,0,-1,-2,0,-5,3,1,1,2,-2,-1,-5,-1] :: [Arithmetic]
titb = [544,0,-32,0, -40,-100,0,-250, 96,40,32,80, -80,-50,-200,-50] :: [Arithmetic]

-- PF/Qstep
-- PF:	!i&1 && !j&1	-> a^2 = 1/4
--		i&1 && j&1		-> b^2/4 = 1/10
--		otherwise		-> ab/2 = 1/4 * sqrt(2/5)
-- kPF[4] = [0.25f, 0.15811388f, 0.15811388f, 0.1f]


-- qstep_base: Qstep values for QP = 0..5 (multiplied by 16)
-- original values: [0.625f, 0.6875f, 0.8125f, 0.875f, 1.0f, 1.125f]
--kQStepBase = [10, 11, 13, 14, 16, 18]



kChromaQP30Map = [29,30,31,32,32,33,34,34,35,35, 36,36,37,37,37,38,38,38,39,39, 39,39]

chromaQpFromLumaWithOffset :: Int -> Int -> Int
chromaQpFromLumaWithOffset qpl ofs
	| qpc < 30 = qpl
	| otherwise = kChromaQP30Map !! (qpl-30)
	where
		qpc = clip3 0 51 (qpl + ofs)




kForwardHadamardTransform = [[1,1,1,1],
						  	 [2,1,-1,-2],
							 [1,-1,-1,1],
							 [1,-2,2,-1]] :: [[Arithmetic]]
		
fastHadamard1D :: [Arithmetic] -> [Arithmetic]
fastHadamard1D xs = map (sum . zipWith (*) xs) kForwardHadamardTransform
-- fastHadamard1D xs = [sum03 + sum12, diff12 + 2*diff03, sum03 - sum12, -(2*diff12) + diff03]
-- 	where
-- 		sum03 = xs !! 0 + xs !! 3
-- 		sum12 = xs !! 1 + xs !! 2
-- 		diff03 = xs !! 0 - xs !! 3
-- 		diff12 = xs !! 1 - xs !! 2


kInverseHadamardTransformDoubled = [[2,2,2,1],
									[2,1,-2,-2],
									[2,-1,-2,2],
									[2,-2,2,-1]] :: [[Arithmetic]]

inverseFastHadamard1DDoubled :: [Arithmetic] -> [Arithmetic]
inverseFastHadamard1DDoubled xs = map (sum . zipWith (*) xs) kInverseHadamardTransformDoubled
-- inverseFastHadamard1D xs = [sum02 + sum13, diff02 + diff13, diff02 - diff13, sum02 - sum13]
-- 	where
-- 		sum02 = xs !! 0 + xs !! 2
-- 		sum13 = xs !! 1 + (xs !! 3) `div` 2
-- 		diff02 = xs !! 0 - xs !! 2
-- 		diff13 = (xs !! 1) `div` 2 - xs !! 3


xx = [5,11,8,10,9,8,4,12,1,10,11,4,19,6,15,7] :: [Arithmetic]
bb = Block4x4 xx

ht4x4Residual :: Arithmetic4x4 -> Arithmetic4x4
ht4x4Residual blk = fromColumns htColumns
	where
		rows = toRows blk
		htRows = map fastHadamard1D rows
		columns = transpose htRows
		htColumns = map fastHadamard1D columns

inverseHT4x4Residual :: Arithmetic4x4 -> Arithmetic4x4
inverseHT4x4Residual blk = fromRaster $ map rescale $ concat rows
	where
		htColumns = toColumns blk
		columns = map inverseFastHadamard1DDoubled htColumns
		htRows = transpose columns
		rows = map inverseFastHadamard1DDoubled htRows
		rescale = \x -> (x+128) `shiftR` 8




-- mf_base: quantizer values for QP = 0..5
-- MF[i] ~= PF[i] * 2^qbits / Qstep
-- [a2, b2d4, abd2]
kMFBase = [[13107, 5243, 8066],
		   [11916, 4660, 7490],
		   [10082, 4194, 6554],
		   [ 9362, 3647, 5825],
		   [ 8192, 3355, 5243],
		   [ 7282, 2893, 4559]] :: [[Arithmetic]]
							 
forwardHTPostScale :: Int -> [Arithmetic]
forwardHTPostScale qp = [a2, abd2, a2, abd2,
						 abd2, b2d4, abd2, b2d4,
						 a2, abd2, a2, abd2,
						 abd2, b2d4, abd2, b2d4]
	where
		[a2, b2d4, abd2] = kMFBase !! (qp `mod` 6)
		

quantize4x4Residual :: Int -> Bool -> Arithmetic4x4 -> Arithmetic4x4
quantize4x4Residual qp isIntra blk = fromRaster $ zipWith quantize mfs $ toRaster blk
	where
		mfs = forwardHTPostScale qp
		qbits = 15 + qp `div` 6
		fq = (1 `shiftL` qbits) `div` (if isIntra then 3 else 6)
		
		quantize :: Arithmetic -> Arithmetic -> Arithmetic
		quantize m x = signum x * ((abs x * m + fq) `shiftR` qbits)


-- v_base: inverse quantizer values for QP = 0..5
-- V[i] ~= Qstep(QP) * PF[i] * 64
-- a2, b2, ab
kVBase = [[10, 16, 13],
		  [11, 18, 14],
		  [13, 20, 16],
		  [14, 23, 18],
		  [16, 25, 20],
		  [18, 29, 23]] :: [[Arithmetic]]

inverseHTRescale :: Int -> [Arithmetic]
inverseHTRescale qp = [a2, ab, a2, ab,
					   ab, b2, ab, b2,
					   a2, ab, a2, ab,
					   ab, b2, ab, b2]
	where
		[a2, b2, ab] = kVBase !! (qp `mod` 6)


rescale4x4Residual :: Int -> Arithmetic4x4 -> Arithmetic4x4
rescale4x4Residual qp blk = fromRaster $ zipWith rescale vs $ toRaster blk
	where
		vs = inverseHTRescale qp
		qbits = qp `div` 6
		
		rescale :: Arithmetic -> Arithmetic -> Arithmetic
		rescale v x = (v*x) `shiftL` qbits


forward4x4Residual :: Int -> Bool -> Arithmetic4x4 -> Arithmetic4x4
forward4x4Residual qp isIntra = (quantize4x4Residual qp isIntra) . ht4x4Residual

inverse4x4Residual :: Int -> Arithmetic4x4 -> Arithmetic4x4
inverse4x4Residual qp = inverseHT4x4Residual . (rescale4x4Residual qp)












