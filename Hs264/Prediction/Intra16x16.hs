-- Hs264.Prediction.Intra16x16

module Hs264.Prediction.Intra16x16 where

import Data.Bits
import Data.List
import Data.Maybe

import Hs264.Types
import Hs264.Block
import Hs264.Functions


intra16x16 :: Neighbors [Sample] -> [Sample16x16]
intra16x16 nb = catMaybes $ map ($ nb) preds
	where
		preds = [intra16x16Vertical,
				 intra16x16Horizontal,
				 intra16x16DC,
				 intra16x16Plane]


-- spec 8.3.3.1
intra16x16Vertical :: Neighbors [Sample] -> Maybe Sample16x16
intra16x16Vertical nb
	| atB nb == Nothing = Nothing
	| otherwise = Just $ fromRows $ replicate 16 $ fromJust $ atB nb


-- spec 8.3.3.2
intra16x16Horizontal :: Neighbors [Sample] -> Maybe Sample16x16
intra16x16Horizontal nb
	| atA nb == Nothing = Nothing
	| otherwise = Just $ fromColumns $ replicate 16 $ fromJust $ atA nb


-- spec 8.3.3.3
intra16x16DC :: Neighbors [Sample] -> Maybe Sample16x16
intra16x16DC nb = Just $ Block16x16 $ replicate 256 dcValue
	where
		dcValue = if atA nb /= Nothing then
					  if atB nb /= Nothing then dcAB else dcA
				  else
					  if atB nb /= Nothing then dcB else dc0
		sumA = sum $ map widen $ fromJust $ atA nb
		sumB = sum $ map widen $ fromJust $ atB nb
		dcAB = narrow $ (sumA + sumB + 16) `shiftR` 5
		dcB = narrow $ (sumB + 8) `shiftR` 4
		dcA = narrow $ (sumA + 8) `shiftR` 4
		dc0 = (1 :: Sample) `shiftL` (bitDepthY-1)


		
-- spec 8.3.3.4
intra16x16Plane :: Neighbors [Sample] -> Maybe Sample16x16
intra16x16Plane nb
	| atA nb == Nothing || atB nb == Nothing || atD nb == Nothing = Nothing
	| otherwise = Just $ Block16x16 fdSamples
	where
		aSamples = map widen $ fromJust $ atA nb
		bSamples = map widen $ fromJust $ atB nb
		dSample = widen $ last $ fromJust $ atD nb
		topSamples = dSample : bSamples
		leftSamples = dSample : aSamples
		
		h = foldr (planeAvg topSamples) 0 [1..8]
		v = foldr (planeAvg leftSamples) 0 [1..8]
		
		a = ((last aSamples) + (last bSamples)) `shiftL` 4
		b = (5*h + 32) `shiftR` 6
		c = (5*v + 32) `shiftR` 6
		
		xDomain = concat $ replicate 16 [0..15]
		yDomain = concat $ map (replicate 16) [0..15]
		fdSamples = zipWith plane xDomain yDomain
		
		planeAvg :: (Integral a) => [a] -> Int -> a -> a
		planeAvg ss x acc = acc + fromIntegral x * (ss !! (8+x) - ss !! (8-x))
		
		plane :: Int -> Int -> Sample
		plane x y = clip1 $ (a + b*(x-7) + c*(y-7) + 16) `shiftR` 5


