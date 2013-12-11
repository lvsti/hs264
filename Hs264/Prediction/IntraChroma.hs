-- Hs264.Prediction.IntraChroma

module Hs264.Prediction.IntraChroma where

import Data.Bits
import Data.List
import Data.Maybe

import Hs264.Types
import Hs264.Block
import Hs264.Functions


nb16 = Neighbors { atA=Just ([0..15] :: [Sample]),
				 atB=Just ([16..31] :: [Sample]),
				 atC=Just ([32..47] :: [Sample]),
				 atD=Just ([99] :: [Sample]) }
nb8 = Neighbors { atA=Just ([0,0,0,0,2,2,2,2] :: [Sample]),
				 atB=Just ([4,4,4,4,6,6,6,6] :: [Sample]),
				 atC=Just ([16..23] :: [Sample]),
				 atD=Just ([99] :: [Sample]) }

gChromaArrayType = 1
gMbWidthC = 8
gMbHeightC = 8


data IntraChromaPredMode = KICPMVertical |
						   KICPMHorizontal |
						   KICPMDC |
						   KICPMPlane
						   deriving (Eq, Ord, Show, Read, Enum, Bounded)


intraChromaForPredMode :: IntraChromaPredMode -> Neighbors [Sample] -> Maybe [Sample]
intraChromaForPredMode pm nb = intraChromaFunction pm $ nb


intraChromaFunction :: IntraChromaPredMode -> (Neighbors [Sample] -> Maybe [Sample])
intraChromaFunction pm = functions !! fromEnum pm
	where
		functions = [intraChromaDC,
					 intraChromaHorizontal,
					 intraChromaVertical,
					 intraChromaPlane]


intraChromaDC :: Neighbors [Sample] -> Maybe [Sample]
intraChromaDC nb = Just fdSamples
	where
		nCols = gMbWidthC `div` 4
		nRows = gMbHeightC `div` 4
		as = map (\n -> fmap ((take 4) . (drop (4*n))) $ atA nb) [0..nRows-1]
		bs = map (\n -> fmap ((take 4) . (drop (4*n))) $ atB nb) [0..nCols-1]
		aDomain = concat $ map (replicate (length bs)) as
		bDomain = concat $ replicate (length as) bs
		microNbs = zipWith (\a b -> Neighbors { atA = a, atB = b, atC = Nothing, atD = Nothing }) aDomain bDomain
		microDCRows = map ((replicate 4) . intraChroma4x4DC) microNbs
		fdSamples = concat $ map (\n -> concat $ replicate 4 $ concat $ take nCols $ drop n microDCRows) [0,2..nRows*nCols-1]
		

-- spec 8.3.4.1
intraChroma4x4DC :: Neighbors [Sample] -> Sample
intraChroma4x4DC nb = dcValue
	where
		dcValue = if atA nb /= Nothing then
					  if atB nb /= Nothing then dcAB else dcA
				  else
					  if atB nb /= Nothing then dcB else dc0
		sumA = sum $ map widen $ fromJust $ atA nb
		sumB = sum $ map widen $ fromJust $ atB nb
		dcAB = narrow $ (sumA + sumB + 4) `shiftR` 3
		dcB = narrow $ (sumB + 2) `shiftR` 2
		dcA = narrow $ (sumA + 2) `shiftR` 2
		dc0 = (1 :: Sample) `shiftL` (bitDepthC-1)


-- spec 8.3.4.2
intraChromaHorizontal :: Neighbors [Sample] -> Maybe [Sample]
intraChromaHorizontal nb
	| atA nb == Nothing = Nothing
	| otherwise = Just $ concat $ transpose $ replicate gMbWidthC $ fromJust $ atA nb



-- spec 8.3.4.3
intraChromaVertical :: Neighbors [Sample] -> Maybe [Sample]
intraChromaVertical nb
	| atB nb == Nothing = Nothing
	| otherwise = Just $ concat $ replicate gMbHeightC $ fromJust $ atB nb

		
-- spec 8.3.4.4
intraChromaPlane :: Neighbors [Sample] -> Maybe [Sample]
intraChromaPlane nb
	| atA nb == Nothing || atB nb == Nothing || atD nb == Nothing = Nothing
	| otherwise = Just fdSamples
	where
		aSamples = map widen $ fromJust $ atA nb
		bSamples = map widen $ fromJust $ atB nb
		dSample = widen $ last $ fromJust $ atD nb
		topSamples = dSample : bSamples
		leftSamples = dSample : aSamples
		
		xcf = if gChromaArrayType == 3 then 4 else 0
		ycf = if gChromaArrayType /= 1 then 4 else 0
		dh = if gChromaArrayType == 3 then 5 else 34
		dv = if gChromaArrayType /= 1 then 5 else 34
		
		h = foldr (planeAvg topSamples xcf) 0 [1..4+xcf]
		v = foldr (planeAvg leftSamples ycf) 0 [1..4+ycf]
		
		a = ((last aSamples) + (last bSamples)) `shiftL` 4
		b = (dh*h + 32) `shiftR` 6
		c = (dv*v + 32) `shiftR` 6
		
		xDomain = concat $ replicate gMbHeightC [0..gMbWidthC-1]
		yDomain = concat $ map (replicate gMbWidthC) [0..gMbHeightC-1]
		fdSamples = zipWith plane xDomain yDomain
		
		planeAvg :: (Integral a) => [a] -> Int -> Int -> a -> a
		planeAvg ss cf x acc = acc + fromIntegral x * (ss !! (3+cf+x) - ss !! (3+cf-x))
		
		plane :: Int -> Int -> Sample
		plane x y = clip1 $ (a + b*(x-3-xcf) + c*(y-3-ycf) + 16) `shiftR` 5


