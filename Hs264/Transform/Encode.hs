-- Hs264.Transform.Encode

module Hs264.Transform.Encode where

import Data.List
import Data.Bits

import Hs264.Transform
import Hs264.Types
import Hs264.Functions
import Hs264.Block





forward4x4Residual :: Int -> Bool -> Arithmetic4x4 -> Arithmetic4x4
forward4x4Residual qp isIntra = (quantize4x4Residual qp isIntra) . ht4x4Residual

forward4x4LumaDC :: Int -> Arithmetic4x4 -> Arithmetic4x4
forward4x4LumaDC qp = (quantize4x4LumaDC qp) . ht4x4LumaDC


-- PF/Qstep
-- PF:	!i&1 && !j&1	-> a^2 = 1/4
--		i&1 && j&1		-> b^2/4 = 1/10
--		otherwise		-> ab/2 = 1/4 * sqrt(2/5)
-- kPF[4] = [0.25f, 0.15811388f, 0.15811388f, 0.1f]



kForwardHTResidual = [[1,1,1,1],
				  	  [2,1,-1,-2],
					  [1,-1,-1,1],
					  [1,-2,2,-1]] :: [[Arithmetic]]
		
forwardHTResidual1D :: [Arithmetic] -> [Arithmetic]
forwardHTResidual1D xs = map (sum . zipWith (*) xs) kForwardHTResidual
-- fastHadamard1D xs = [sum03 + sum12, diff12 + 2*diff03, sum03 - sum12, -(2*diff12) + diff03]
-- 	where
-- 		sum03 = xs !! 0 + xs !! 3
-- 		sum12 = xs !! 1 + xs !! 2
-- 		diff03 = xs !! 0 - xs !! 3
-- 		diff12 = xs !! 1 - xs !! 2

ht4x4Residual :: Arithmetic4x4 -> Arithmetic4x4
ht4x4Residual blk = fromColumns htColumns
	where
		rows = toRows blk
		htRows = map forwardHTResidual1D rows
		columns = transpose htRows
		htColumns = map forwardHTResidual1D columns



kForwardHTLumaDC = [[1,1,1,1],
					[1,1,-1,-1],
					[1,-1,-1,1],
					[1,-1,1,-1]] :: [[Arithmetic]]

forwardHTLumaDC1D :: [Arithmetic] -> [Arithmetic]
forwardHTLumaDC1D xs = map (sum . zipWith (*) xs) kForwardHTLumaDC

ht4x4LumaDC :: Arithmetic4x4 -> Arithmetic4x4
ht4x4LumaDC blk = fromColumns htColumns
	where
		rows = toRows blk
		htRows = map forwardHTLumaDC1D rows
		columns = transpose htRows
		htColumns = map ((map (`div` 2)) . forwardHTLumaDC1D) columns



kForwardHTChromaDC = [[1,1],
					  [1,-1]] :: [[Arithmetic]]

forwardHTChromaDC1D :: [Arithmetic] -> [Arithmetic]
forwardHTChromaDC1D xs = map (sum . zipWith (*) xs) kForwardHTChromaDC

ht2x2ChromaDC :: Arithmetic2x2 -> Arithmetic2x2
ht2x2ChromaDC blk = fromColumns htColumns
	where
		rows = toRows blk
		htRows = map forwardHTChromaDC1D rows
		columns = transpose htRows
		htColumns = map forwardHTChromaDC1D columns



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
quantize4x4Residual qp isIntra blk = fromRaster $ zipWith quantizeResidual mfs $ toRaster blk
	where
		mfs = forwardHTPostScale qp
		qbits = 15 + qp `div` 6
		fq = (1 `shiftL` qbits) `div` (if isIntra then 3 else 6)
		
		quantizeResidual :: Arithmetic -> Arithmetic -> Arithmetic
		quantizeResidual m x = signum x * ((abs x * m + fq) `shiftR` qbits)


quantize4x4LumaDC :: Int -> Arithmetic4x4 -> Arithmetic4x4
quantize4x4LumaDC qp blk = fmap quantizeLumaDC blk
	where
		mfs = forwardHTPostScale qp
		mf0 = mfs !! 0
		qbits1 = 15 + qp `div` 6 + 1
		fq2 = (1 `shiftL` qbits1) `div` 3
		
		quantizeLumaDC :: Arithmetic -> Arithmetic
		quantizeLumaDC x = signum x * ((abs x * mf0 + fq2) `shiftR` qbits1)


quantize2x2ChromaDC :: Int -> Arithmetic4x4 -> Arithmetic4x4
quantize2x2ChromaDC qp blk = fmap quantizeChromaDC blk
	where
		mfs = forwardHTPostScale qp
		mf0 = mfs !! 0
		qbits1 = 15 + qp `div` 6 + 1
		fq2 = (1 `shiftL` qbits1) `div` 3
		
		quantizeChromaDC :: Arithmetic -> Arithmetic
		quantizeChromaDC x = signum x * ((abs x * mf0 + fq2) `shiftR` qbits1)







