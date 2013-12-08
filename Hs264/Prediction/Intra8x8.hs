-- Hs264.Prediction.Intra8x8

module Hs264.Prediction.Intra8x8 where

import Data.Bits
import Data.List
import Data.Maybe

import Hs264.Types
import Hs264.Block
import Hs264.Functions


data Intra8x8PredMode = KI8x8PMVertical |
						KI8x8PMHorizontal |
						KI8x8PMDC |
						KI8x8PMDiagonalDownLeft |
						KI8x8PMDiagonalDownRight |
						KI8x8PMVerticalRight |
						KI8x8PMHorizontalDown |
						KI8x8PMVerticalLeft |
						KI8x8PMHorizontalUp
						deriving (Eq, Ord, Show, Read, Enum, Bounded)


intra8x8ForPredMode :: Intra8x8PredMode -> Neighbors [Sample] -> Maybe Sample8x8
intra8x8ForPredMode pm nb = intra8x8Function pm $ nb


intra8x8Function :: Intra8x8PredMode -> (Neighbors [Sample] -> Maybe Sample8x8)
intra8x8Function pm = functions !! fromEnum pm
	where
		functions = [intra8x8Vertical,
					 intra8x8Horizontal,
					 intra8x8DC,
					 intra8x8DiagonalDownLeft,
					 intra8x8DiagonalDownRight,
					 intra8x8VerticalRight,
					 intra8x8HorizontalDown,
					 intra8x8VerticalLeft,
					 intra8x8HorizontalUp]


-- spec 8.3.2.2.1
i8x8Filter :: Neighbors [Sample] -> Neighbors [Sample]
i8x8Filter nb = Neighbors { atA = maybeFdA, atB = maybeFdB, atC = maybeFdC, atD = maybeFdD}
	where
		maybeFdA = filterAwD (atA nb) (atD nb)
		(maybeFdB, maybeFdC) = filterBCwD (atB nb) (atC nb) (atD nb)
		maybeFdD = filterDwAB (atD nb) (atA nb) (atB nb)
		
		filterAwD :: Maybe [Sample] -> Maybe [Sample] -> Maybe [Sample]
		filterAwD Nothing _ = Nothing
		filterAwD (Just as) md = Just fdSamples
			where
				aSamples = map widen as
				dSample = if md == Nothing then head aSamples else widen $ last $ fromJust md
				samples = dSample : aSamples ++ [last aSamples]
				fdSamples = map (narrow . (`shiftR` 2) . (+2)) $ fst $ sfilter1D [1,2,1] samples
		
		filterDwAB :: Maybe [Sample] -> Maybe [Sample] -> Maybe [Sample] -> Maybe [Sample]
		filterDwAB Nothing _ _ = Nothing
		filterDwAB (Just ds) ma mb = Just fdSamples
			where
				dSample = widen $ last ds
				aSample = if ma == Nothing then dSample else widen $ head $ fromJust ma
				bSample = if mb == Nothing then dSample else widen $ head $ fromJust mb
				fdSamples = [narrow $ (aSample + 2*dSample + bSample + 2) `shiftR` 2]

		filterBCwD :: Maybe [Sample] -> Maybe [Sample] -> Maybe [Sample] -> (Maybe [Sample], Maybe [Sample])
		filterBCwD Nothing mc _ = (Nothing, mc)
		filterBCwD mb Nothing _ = (mb, Nothing)
		filterBCwD (Just bs) (Just cs) md = (Just fdBSamples, Just fdCSamples)
			where
				bSamples = map widen bs
				cSamples = map widen cs
				dSample = if md == Nothing then head bSamples else widen $ last $ fromJust md
				samples = dSample : bSamples ++ cSamples ++ [last cSamples]
				fdSamples = map (narrow . (`shiftR` 2) . (+2)) $ fst $ sfilter1D [1,2,1] samples
				(fdBSamples, fdCSamples) = splitAt 8 fdSamples


-- spec 8.3.2.2.2
intra8x8Vertical :: Neighbors [Sample] -> Maybe Sample8x8
intra8x8Vertical nb
	| atB nb == Nothing = Nothing
	| otherwise = Just $ fromRows $ replicate 8 $ fromJust $ atB nb


-- spec 8.3.2.2.3
intra8x8Horizontal :: Neighbors [Sample] -> Maybe Sample8x8
intra8x8Horizontal nb
	| atA nb == Nothing = Nothing
	| otherwise = Just $ fromColumns $ replicate 8 $ fromJust $ atA nb


-- spec 8.3.2.2.4
intra8x8DC :: Neighbors [Sample] -> Maybe Sample8x8
intra8x8DC nb = Just $ Block8x8 $ replicate 64 dcValue
	where
		dcValue = if atA nb /= Nothing then
					  if atB nb /= Nothing then dcAB else dcA
				  else
					  if atB nb /= Nothing then dcB else dc0
		sumA = sum $ map widen $ fromJust $ atA nb
		sumB = sum $ map widen $ fromJust $ atB nb
		dcAB = narrow $ (sumA + sumB + 8) `shiftR` 4
		dcB = narrow $ (sumB + 4) `shiftR` 3
		dcA = narrow $ (sumA + 4) `shiftR` 3
		dc0 = (1 :: Sample) `shiftL` (bitDepthY-1)
		
		
-- spec 8.3.2.2.5
intra8x8DiagonalDownLeft :: Neighbors [Sample] -> Maybe Sample8x8
intra8x8DiagonalDownLeft nb
	| atB nb == Nothing || atC nb == Nothing = Nothing
	| otherwise = Just $ Block8x8 fdSamples
	where
		bSamples = map widen $ fromJust $ atB nb
		cSamples = map widen $ fromJust $ atC nb
		topSamples = bSamples ++ cSamples ++ [last cSamples]
		
		fdTopSamples = map (narrow . (`shiftR` 2) . (+2)) $ fst $ sfilter1D [1,2,1] topSamples
		fdSamples = snd $ foldr take8ShiftL1 (fdTopSamples,[]) [0..7]
		
		take8ShiftL1 :: a -> ([b],[b]) -> ([b],[b])
		take8ShiftL1 _ (ss@(_:stail), acc) = (stail, acc ++ take 8 ss)


-- spec 8.3.2.2.6
intra8x8DiagonalDownRight :: Neighbors [Sample] -> Maybe Sample8x8
intra8x8DiagonalDownRight nb
	| atA nb == Nothing || atB nb == Nothing || atD nb == Nothing = Nothing
	| otherwise = Just $ Block8x8 fdSamples
	where
		aSamples = map widen $ fromJust $ atA nb
		bSamples = map widen $ fromJust $ atB nb
		dSample = widen $ last $ fromJust $ atD nb
		topSamples = reverse aSamples ++ (dSample : bSamples)
		
		fdTopSamples = map (narrow . (`shiftR` 2) . (+2)) $ fst $ sfilter1D [1,2,1] topSamples
		fdSamples = foldr (\n acc -> (take 8 $ drop (7-n) fdTopSamples) ++ acc) [] [0..7]
		

-- spec 8.3.2.2.7
intra8x8VerticalRight :: Neighbors [Sample] -> Maybe Sample8x8
intra8x8VerticalRight nb
	| atA nb == Nothing || atB nb == Nothing || atD nb == Nothing = Nothing
	| otherwise = Just $ Block8x8 fdSamples
	where
		aSamples = map widen $ fromJust $ atA nb
		bSamples = map widen $ fromJust $ atB nb
		dSample = widen $ last $ fromJust $ atD nb
		evenSamples = dSample : bSamples
		oddSamples = tail (reverse aSamples) ++ evenSamples
		
		fdEvenSamples = map (narrow . (`shiftR` 1) . (+1)) $ fst $ sfilter1D [1,1] evenSamples
		(fdM7:fdM6:fdM5:fdM4:fdM3:fdM2:fdM1:fdOdds) = map (narrow . (`shiftR` 2) . (+2)) $ fst $ sfilter1D [1,2,1] oddSamples
		
		fdSamples = fdEvenSamples ++ 
					(fdM1 : fdOdds) ++ 
					(take 8 (fdM2 : fdEvenSamples)) ++ 
					(take 8 (fdM3 : fdM1 : fdOdds)) ++
					(take 8 (fdM4 : fdM2 : fdEvenSamples)) ++
					(take 8 (fdM5 : fdM3 : fdM1 : fdOdds)) ++
					(take 8 (fdM6 : fdM4 : fdM2 : fdEvenSamples)) ++
					(take 8 (fdM7 : fdM5 : fdM3 : fdM1 : fdOdds))
		

-- spec 8.3.2.2.8
intra8x8HorizontalDown :: Neighbors [Sample] -> Maybe Sample8x8
intra8x8HorizontalDown nb = maybeSamples
	where
		transposedNb = Neighbors { atA = atB nb, atB = atA nb, atC = atC nb, atD = atD nb }
		maybeTpSamples = intra8x8VerticalRight transposedNb
		maybeSamples = if maybeTpSamples == Nothing then
						   Nothing
					   else
						   Just $ fromRows $ transpose $ toRows $ fromJust maybeTpSamples


-- spec 8.3.2.2.9
intra8x8VerticalLeft :: Neighbors [Sample] -> Maybe Sample8x8
intra8x8VerticalLeft nb
	| atB nb == Nothing || atC nb == Nothing = Nothing
	| otherwise = Just $ Block8x8 fdSamples
	where
		bSamples = map widen $ fromJust $ atB nb
		cSamples = map widen $ fromJust $ atC nb
		topSamples = bSamples ++ cSamples
		
		fdEvenSamples = map (narrow . (`shiftR` 1) . (+1)) $ fst $ sfilter1D [1,1] topSamples
		fdOddSamples = map (narrow . (`shiftR` 2) . (+2)) $ fst $ sfilter1D [1,2,1] topSamples
		
		fdSampleRows = [fdEvenSamples,
						fdOddSamples,
						tail fdEvenSamples,
						tail fdOddSamples,
						tail $ tail fdEvenSamples,
						tail $ tail fdOddSamples,
						tail $ tail $ tail fdEvenSamples,
						tail $ tail $ tail fdOddSamples]
		fdSamples = concat $ map (take 8) fdSampleRows


-- spec 8.3.2.2.10
intra8x8HorizontalUp :: Neighbors [Sample] -> Maybe Sample8x8
intra8x8HorizontalUp nb
	| atA nb == Nothing = Nothing
	| otherwise = Just $ Block8x8 fdSamples
	where
		aSamples = map widen $ fromJust $ atA nb
		lastA = last aSamples
		extSample = narrow lastA
		evenSamples = aSamples
		oddSamples = aSamples ++ [lastA]
		
		fdEvenSamples = map (narrow . (`shiftR` 1) . (+1)) $ fst $ sfilter1D [1,1] evenSamples
		fdOddSamples = map (narrow . (`shiftR` 2) . (+2)) $ fst $ sfilter1D [1,2,1] oddSamples
		
		fdSampleColumns = [pad8 fdEvenSamples,
			 			   pad8 fdOddSamples,
						   pad8 $ tail fdEvenSamples,
						   pad8 $ tail fdOddSamples,
						   pad8 $ tail $ tail fdEvenSamples,
						   pad8 $ tail $ tail fdOddSamples,
						   pad8 $ tail $ tail $ tail fdEvenSamples,
						   pad8 $ tail $ tail $ tail fdOddSamples]
		
		fdSamples = (concat . transpose) fdSampleColumns

		pad8 :: [Sample] -> [Sample]
		pad8 xs = take 8 (xs ++ repeat extSample)
