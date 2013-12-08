-- Hs264.Prediction.Intra4x4

module Hs264.Prediction.Intra4x4 where

import Data.Bits
import Data.List
import Data.Maybe

import Hs264.Types
import Hs264.Block
import Hs264.Functions


intra4x4 :: Neighbors [Sample] -> [Sample4x4]
intra4x4 nb = catMaybes $ map ($ nb) preds
	where
		preds = [intra4x4Vertical,
				 intra4x4Horizontal,
				 intra4x4DC,
				 intra4x4DiagonalDownLeft,
				 intra4x4DiagonalDownRight,
				 intra4x4VerticalRight,
				 intra4x4HorizontalDown,
				 intra4x4VerticalLeft,
				 intra4x4HorizontalUp]


-- spec 8.3.1.2.1
intra4x4Vertical :: Neighbors [Sample] -> Maybe Sample4x4
intra4x4Vertical nb
	| atB nb == Nothing = Nothing
	| otherwise = Just $ fromRows $ replicate 4 $ fromJust $ atB nb


-- spec 8.3.1.2.2
intra4x4Horizontal :: Neighbors [Sample] -> Maybe Sample4x4
intra4x4Horizontal nb
	| atA nb == Nothing = Nothing
	| otherwise = Just $ fromColumns $ replicate 4 $ fromJust $ atA nb


-- spec 8.3.1.2.3
intra4x4DC :: Neighbors [Sample] -> Maybe Sample4x4
intra4x4DC nb = Just $ Block4x4 $ replicate 16 dcValue
	where
		dcValue = if atA nb /= Nothing then
					  if atB nb /= Nothing then dcAB else dcA
				  else
					  if atB nb /= Nothing then dcB else dc0
		sumA = sum $ map widen (fromJust $ atA nb)
		sumB = sum $ map widen (fromJust $ atB nb)
		dcAB = narrow $ (sumA + sumB + 4) `shiftR` 3
		dcB = narrow $ (sumB + 2) `shiftR` 2
		dcA = narrow $ (sumA + 2) `shiftR` 2
		dc0 = (1 :: Sample) `shiftL` (bitDepthY-1)
		
		
-- spec 8.3.1.2.4
intra4x4DiagonalDownLeft :: Neighbors [Sample] -> Maybe Sample4x4
intra4x4DiagonalDownLeft nb
	| atB nb == Nothing || atC nb == Nothing = Nothing
	| otherwise = Just $ Block4x4 fdSamples
	where
		bSamples = map widen $ fromJust $ atB nb
		cSamples = map widen $ fromJust $ atC nb
		topSamples = bSamples ++ cSamples ++ [last cSamples]
		
		fdTopSamples = map (narrow . (`shiftR` 2) . (+2)) $ fst $ sfilter1D [1,2,1] topSamples
		fdSamples = snd $ foldr take4ShiftL1 (fdTopSamples,[]) [0..3]
		
		take4ShiftL1 :: a -> ([b],[b]) -> ([b],[b])
		take4ShiftL1 _ (s1:stail@(s2:s3:s4:ss), acc) = (stail, acc ++ [s1,s2,s3,s4])


-- spec 8.3.1.2.5
intra4x4DiagonalDownRight :: Neighbors [Sample] -> Maybe Sample4x4
intra4x4DiagonalDownRight nb
	| atA nb == Nothing || atB nb == Nothing || atD nb == Nothing = Nothing
	| otherwise = Just $ Block4x4 fdSamples
	where
		aSamples = map widen $ fromJust $ atA nb
		bSamples = map widen $ fromJust $ atB nb
		dSample = widen $ last $ fromJust $ atD nb
		topSamples = reverse aSamples ++ (dSample : bSamples)
		
		fdTopSamples = map (narrow . (`shiftR` 2) . (+2)) $ fst $ sfilter1D [1,2,1] topSamples
		fdSamples = foldr (\n acc -> (take 4 $ drop (3-n) fdTopSamples) ++ acc) [] [0..3]
		

-- spec 8.3.1.2.6
intra4x4VerticalRight :: Neighbors [Sample] -> Maybe Sample4x4
intra4x4VerticalRight nb
	| atA nb == Nothing || atB nb == Nothing || atD nb == Nothing = Nothing
	| otherwise = Just $ Block4x4 fdSamples
	where
		[a0,a1,a2,_] = map widen $ fromJust $ atA nb
		bSamples = map widen $ fromJust $ atB nb
		dSample = widen $ last $ fromJust $ atD nb
		evenSamples = dSample : bSamples
		oddSamples = a2 : a1 : a0 : evenSamples
		
		fdEvenSamples = map (narrow . (`shiftR` 1) . (+1)) $ fst $ sfilter1D [1,1] evenSamples
		(fdM3:fdM2:fdOdds) = map (narrow . (`shiftR` 2) . (+2)) $ fst $ sfilter1D [1,2,1] oddSamples
		
		fdSamples = fdEvenSamples ++ 
					fdOdds ++ 
					(fdM2 : (take 3 fdEvenSamples)) ++ 
					(fdM3 : (take 3 fdOdds))
		

-- spec 8.3.1.2.7
intra4x4HorizontalDown :: Neighbors [Sample] -> Maybe Sample4x4
intra4x4HorizontalDown nb = maybeSamples
	where
		transposedNb = Neighbors { atA = atB nb, atB = atA nb, atC = atC nb, atD = atD nb }
		maybeTpSamples = intra4x4VerticalRight transposedNb
		maybeSamples = if maybeTpSamples == Nothing then
						   Nothing
					   else
						   Just $ fromRows $ transpose $ toRows $ fromJust maybeTpSamples


-- spec 8.3.1.2.8
intra4x4VerticalLeft :: Neighbors [Sample] -> Maybe Sample4x4
intra4x4VerticalLeft nb
	| atB nb == Nothing || atC nb == Nothing = Nothing
	| otherwise = Just $ Block4x4 fdSamples
	where
		bSamples = map widen $ fromJust $ atB nb
		cSamples = map widen $ fromJust $ atC nb
		topSamples = bSamples ++ cSamples
		
		fdEvenSamples = map (narrow . (`shiftR` 1) . (+1)) $ fst $ sfilter1D [1,1] topSamples
		fdOddSamples = map (narrow . (`shiftR` 2) . (+2)) $ fst $ sfilter1D [1,2,1] topSamples
		
		fdSamples = concat $ map (take 4) [fdEvenSamples, fdOddSamples, tail fdEvenSamples, tail fdOddSamples]


-- spec 8.3.1.2.9
intra4x4HorizontalUp :: Neighbors [Sample] -> Maybe Sample4x4
intra4x4HorizontalUp nb
	| atA nb == Nothing = Nothing
	| otherwise = Just $ Block4x4 fdSamples
	where
		aSamples = map widen $ fromJust $ atA nb
		lastA = last aSamples
		extSample = narrow lastA
		evenSamples = aSamples
		oddSamples = aSamples ++ [lastA]
		
		fdEvenSamples = map (narrow . (`shiftR` 1) . (+1)) $ fst $ sfilter1D [1,1] evenSamples
		fdOddSamples = map (narrow . (`shiftR` 2) . (+2)) $ fst $ sfilter1D [1,2,1] oddSamples
		
		fdSampleColumns = [fdEvenSamples ++ [extSample],
			 			   fdOddSamples ++ [extSample],
						   tail fdEvenSamples ++ [extSample, extSample],
						   tail fdOddSamples ++ [extSample, extSample]]
		
		fdSamples = (concat . transpose) fdSampleColumns
