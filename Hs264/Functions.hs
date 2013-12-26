-- Hs264.Functions

module Hs264.Functions where

import Data.List

import Hs264.Types



-- GENERAL
clip3 :: (Integral a, Integral b) => a -> a -> b -> a
clip3 cmin cmax value
	| vmin <= 0 = cmin
	| vmax >= 0 = cmax
	| otherwise = cmin + fromIntegral value - cmin
	where
		vmin = value - fromIntegral cmin
		vmax = value - fromIntegral cmax

clip1 :: (Integral a) => a -> Sample
clip1 value = clip3 low high value
	where
		low = minBound :: Sample
		high = maxBound :: Sample

inverseRasterScanX :: Int -> Int -> Int -> Int -> Int
inverseRasterScanX a b _ d = (a `mod` (d `div` b)) * b

inverseRasterScanY :: Int -> Int -> Int -> Int -> Int
inverseRasterScanY a b c d = (a `div` (d `div` b)) * c

median :: (Num a, Ord a) => a -> a -> a -> a
median x y z = x + y + z - (min x (min y z)) - (max x (max y z))


sfilter1D :: (Num a) => [a] -> [a] -> ([a],a)
sfilter1D kernel samples
	| kwidth <= swidth = (unnormedSamples, sum kernel)
	| otherwise = error "kernel overflows sample array"
	where
		swidth = length samples
		kwidth = length kernel
		unnormedSamples = snd $ foldr kernelStep (samples, replicate swidth 0) kernel
							
		kernelStep :: (Num a) => a -> ([a],[a]) -> ([a],[a])
		kernelStep k (ss@(shead:stail), acc) = (stail, zipWith (\s a -> s*k + a) ss acc)

dotProduct :: (Num a) => [a] -> [a] -> a
dotProduct v1 v2 = sum $ zipWith (*) v1 v2


-- 4x4 BLOCKS
{-
blk4x4ToRows :: [a] -> [[a]]
blk4x4ToRows xs = map (\i -> take 4 $ drop (4*i) xs) [0..3]

blk4x4ToColumns :: [a] -> [[a]]
blk4x4ToColumns = transpose . blk4x4ToRows

blk4x4Raster :: [a] -> [a]
blk4x4Raster = id

blk4x4Zigzag :: [a] -> [a]
blk4x4Zigzag xs = map (\i -> xs !! i) zigzagScan
	where
		zigzagScan = [0,1,4,8,5,2,3,6,9,12,13,10,7,11,14,15]

blk4x4Morton :: [a] -> [a]
blk4x4Morton xs = map (\i -> xs !! i) mortonScan
	where
		mortonScan = [0,1,4,5,2,3,6,7,8,9,12,13,10,11,14,15]

blk4x4FromRows :: [[a]] -> [a]
blk4x4FromRows = concat

blk4x4FromColumns :: [[a]] -> [a]
blk4x4FromColumns = blk4x4FromRows . transpose


-}

