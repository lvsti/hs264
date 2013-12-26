-- Hs264.Transform

module Hs264.Transform where

import Data.List
import Data.Bits

import Hs264.Types
import Hs264.Functions
import Hs264.Block




-- public API
inverse4x4Residual :: Int -> Arithmetic4x4 -> Arithmetic4x4
inverse4x4Residual qp = inverseHT4x4Residual . (rescale4x4Residual qp)

inverse4x4LumaDC :: Int -> Arithmetic4x4 -> Arithmetic4x4
inverse4x4LumaDC qp = (rescale4x4LumaDC qp) . inverseHT4x4LumaDC

inverse8x8Residual :: Int -> Arithmetic8x8 -> Arithmetic8x8
inverse8x8Residual qp = inverseHT8x8Residual . (rescale8x8Residual qp)

inverse2x2ChromaDC :: Int -> Arithmetic2x2 -> Arithmetic2x2
inverse2x2ChromaDC qp = (rescale2x2ChromaDC qp) . inverseHT2x2ChromaDC

inverse2x4ChromaDC :: Int -> Arithmetic2x4 -> Arithmetic2x4
inverse2x4ChromaDC qp = (rescale2x4ChromaDC qp) . inverseHT2x4ChromaDC




-- spec 8.5.8
kChromaQP30Map = [29,30,31,32,32,33,34,34,35,35, 36,36,37,37,37,38,38,38,39,39, 39,39]

chromaQPFromLuma :: Int -> Int -> Int -> Int
chromaQPFromLuma crQpBdOfs qpLuma crQpOfs
	| qpi < 30 = qpi + crQpBdOfs
	| otherwise = (kChromaQP30Map !! (qpi-30)) + crQpBdOfs
	where
		qpi = clip3 (-crQpBdOfs) 51 (qpLuma + crQpOfs)



-- spec 8.5.12.2
inverseHT4x4Residual1D :: [Arithmetic] -> [Arithmetic]
inverseHT4x4Residual1D = secondPass . firstPass
	where
		firstPass :: [Arithmetic] -> [Arithmetic]
		firstPass [d0,d1,d2,d3] = [e0,e1,e2,e3]
			where
				e0 = d0 + d2
				e1 = d0 - d2
				e2 = (d1 `shiftR` 1) - d3
				e3 = d1 + (d3 `shiftR` 1)
				
		secondPass :: [Arithmetic] -> [Arithmetic]
		secondPass [e0,e1,e2,e3] = [f0,f1,f2,f3]
			where
				f0 = e0 + e3
				f1 = e1 + e2
				f2 = e1 - e2
				f3 = e0 - e3

inverseHT4x4Residual :: Arithmetic4x4 -> Arithmetic4x4
inverseHT4x4Residual blk = fromRaster $ map rescale $ concat rows
	where
		htColumns = toColumns blk
		columns = map inverseHT4x4Residual1D htColumns
		htRows = transpose columns
		rows = map inverseHT4x4Residual1D htRows
		rescale = \x -> (x+32) `shiftR` 6



-- spec 8.5.13.2
inverseHT8x8Residual1D :: [Arithmetic] -> [Arithmetic]
inverseHT8x8Residual1D xs = [g0,g1,g2,g3,g4,g5,g6,g7]
	where
		[f0,f1,f2,f3,f4,f5,f6,f7] = secondPass . firstPass $ xs
		g0 = f0 + f7
		g1 = f2 + f5
		g2 = f4 + f3
		g3 = f6 + f1
		g4 = f6 - f1
		g5 = f4 - f3
		g6 = f2 - f5
		g7 = f0 - f7
		
		firstPass :: [Arithmetic] -> [Arithmetic]
		firstPass [d0,d1,d2,d3,d4,d5,d6,d7] = [e0,e1,e2,e3,e4,e5,e6,e7]
			where
				e0 = d0 + d4
				e1 = -d3 + d5 - d7 - (d7 `shiftR` 1)
				e2 = d0 - d4
				e3 = d1 + d7 - d3 - (d3 `shiftR` 1)
				e4 = (d2 `shiftR` 1) - d6
				e5 = -d1 + d7 + d5 + (d5 `shiftR` 1)
				e6 = d2 + (d6 `shiftR` 1)
				e7 = d3 + d5 + d1 + (d1 `shiftR` 1)
		
		secondPass :: [Arithmetic] -> [Arithmetic]
		secondPass [e0,e1,e2,e3,e4,e5,e6,e7] = [f0,f1,f2,f3,f4,f5,f6,f7]
			where
				f0 = e0 + e6
				f1 = e1 + (e7 `shiftR` 2)
				f2 = e2 + e4
				f3 = e3 + (e5 `shiftR` 2)
				f4 = e2 - e4
				f5 = (e3 `shiftR` 2) - e5
				f6 = e0 - e6
				f7 = e7 - (e1 `shiftR` 2)


inverseHT8x8Residual :: Arithmetic8x8 -> Arithmetic8x8
inverseHT8x8Residual blk = fromRaster $ map rescale $ concat rows
	where
		htColumns = toColumns blk
		columns = map inverseHT8x8Residual1D htColumns
		htRows = transpose columns
		rows = map inverseHT8x8Residual1D htRows
		rescale = \x -> (x+32) `shiftR` 6


-- spec 8.5.10
kInverseHT4x4LumaDC = [[1,1,1,1],
					   [1,1,-1,-1],
					   [1,-1,-1,1],
					   [1,-1,1,-1]] :: [[Arithmetic]]

inverseHT4x4LumaDC1D :: [Arithmetic] -> [Arithmetic]
inverseHT4x4LumaDC1D xs = map (dotProduct xs) kInverseHT4x4LumaDC

inverseHT4x4LumaDC :: Arithmetic4x4 -> Arithmetic4x4
inverseHT4x4LumaDC blk = fromRows rows
	where
		htColumns = toColumns blk
		columns = map inverseHT4x4LumaDC1D htColumns
		htRows = transpose columns
		rows = map inverseHT4x4LumaDC1D htRows


-- spec 8.5.11.1
kInverseHT2x2ChromaDC = [[1,1],
						 [1,-1]] :: [[Arithmetic]]

inverseHT2x2ChromaDC1D :: [Arithmetic] -> [Arithmetic]
inverseHT2x2ChromaDC1D xs = map (dotProduct xs) kInverseHT2x2ChromaDC

inverseHT2x2ChromaDC :: Arithmetic2x2 -> Arithmetic2x2
inverseHT2x2ChromaDC blk = fromRows rows
	where
		htColumns = toColumns blk
		columns = map inverseHT2x2ChromaDC1D htColumns
		htRows = transpose columns
		rows = map inverseHT2x2ChromaDC1D htRows


-- spec 8.5.11.1
kInverseHT2x4ChromaDCLeft = [[1,1,1,1],
							 [1,1,-1,-1],
							 [1,-1,-1,1],
							 [1,-1,1,-1]] :: [[Arithmetic]]
kInverseHT2x4ChromaDCRight = [[1,1],
						  	  [1,-1]] :: [[Arithmetic]]

inverseHT2x4ChromaDC :: Arithmetic2x4 -> Arithmetic2x4
inverseHT2x4ChromaDC blk = fromRows rows
	where
		htColumns = toColumns blk
		htRows = foldr (\lts acc -> map (dotProduct lts) htColumns : acc) [] kInverseHT2x4ChromaDCLeft
		rows = foldr (\rs acc -> map (dotProduct rs) kInverseHT2x4ChromaDCRight : acc) [] htRows


-- spec 8.5.9

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


-- v0..v5
kVBase8x8 = [[20, 18, 32, 19, 25, 24],
		  	 [22, 19, 35, 21, 28, 26],
		  	 [26, 23, 42, 24, 33, 31],
		  	 [28, 25, 45, 26, 35, 33],
		  	 [32, 28, 51, 30, 40, 38],
		  	 [36, 32, 58, 34, 46, 43]] :: [[Arithmetic]]
		
inverseHT8x8Rescale :: Int -> [Arithmetic]
inverseHT8x8Rescale qp = concat [hrow0, hrow0,
								 hrow1, hrow1,
								 hrow2, hrow2,
								 hrow3, hrow3]
	where
		[v0, v1, v2, v3, v4, v5] = kVBase8x8 !! (qp `mod` 6)
		hrow0 = [v0, v3, v4, v3]
		hrow1 = [v3, v1, v5, v1]
		hrow2 = [v4, v5, v2, v5]
		hrow3 = hrow1


-- spec 8.5.12.1
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


-- spec 8.5.13.1
rescale8x8Residual :: Int -> Arithmetic8x8 -> Arithmetic8x8
rescale8x8Residual qp blk = fromRaster $ zipWith rescaleResidual vs $ toRaster blk
	where
		vs = inverseHT8x8Rescale qp
		qpd6 = qp `div` 6
		rescaleResidual = if qp >= 36 then rescaleResidual8x8HighQP else rescaleResidual8x8LowQP
				
		rescaleResidual8x8HighQP :: Arithmetic -> Arithmetic -> Arithmetic
		rescaleResidual8x8HighQP v x = (v*x) `shiftL` qpShift
			where
				qpShift = qpd6 - 6

		rescaleResidual8x8LowQP :: Arithmetic -> Arithmetic -> Arithmetic
		rescaleResidual8x8LowQP v x = (v*x + fq) `shiftR` qpShift
			where
				fq = 1 `shiftL` (5-qpd6)
				qpShift = 6 - qpd6


-- spec 8.5.10
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


-- spec 8.5.11.2
rescale2x2ChromaDC :: Int -> Arithmetic2x2 -> Arithmetic2x2
rescale2x2ChromaDC qp blk = fmap rescaleChromaDC420 blk
	where
		vs = inverseHT4x4Rescale qp
		v0 = vs !! 0
		qpd6 = qp `div` 6
		
		rescaleChromaDC420 :: Arithmetic -> Arithmetic
		rescaleChromaDC420 x = ((v0*x) `shiftL` qpd6) `shiftR` 5
		

-- spec 8.5.11.2
rescale2x4ChromaDC :: Int -> Arithmetic2x4 -> Arithmetic2x4
rescale2x4ChromaDC qp blk = fmap rescaleChromaDC blk
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











