-- Hs264.Block

module Hs264.Block where

import Data.List


--kZigzagScan = [0,1,4,8,5,2,3,6,9,12,13,10,7,11,14,15] :: [Int]
--kZigzagScanInverse = [0,1,5,6,2,4,7,12,3,8,11,13,9,10,14,15] :: [Int]
--kMortonScan = [0,1,4,5,2,3,6,7,8,9,12,13,10,11,14,15] :: [Int]
--kMortonScan = [0,1,2,3] :: [Int]


class Block block where
	width :: block a -> Int
	height :: block a -> Int

	toRaster :: block a -> [a]
	fromRaster :: [a] -> block a

	toRows :: block a -> [[a]]
	toRows blk = map (\i -> take w $ drop (w*i) xs) $ [0..h-1]
		where
			xs = toRaster blk
			w = width blk
			h = height blk

	toColumns :: block a -> [[a]]
	toColumns = transpose . toRows
	
	fromRows :: [[a]] -> block a
	fromRows = fromRaster . concat

	fromColumns :: [[a]] -> block a
	fromColumns = fromRows . transpose
	

	zigzagScan :: block a -> [Int]
	zigzagScan blk = nextZigzagIndex 0 0 0
		where
			w = width blk
			h = height blk
			maxIndex = w*h - 1
			
			nextZigzagIndex :: Int -> Int -> Int -> [Int]
			nextZigzagIndex x y d = currentIndex : next
				where
					currentIndex = y*w + x
					next = if currentIndex == maxIndex then
							   []
						   else
							   if even d then
								   let
								   	   d1 = if y == 0 || x == w-1 then d+1 else d
									   x1 = if x == w-1 then x else x+1
									   y1 = if x == w-1 then y+1 else (if y == 0 then y else y-1)
								   in
								   	   nextZigzagIndex x1 y1 d1
							   else
								   let
								   	   d1 = if y == h-1 || x == 0 then d+1 else d
									   x1 = if y == h-1 then x+1 else (if x == 0 then x else x-1)
									   y1 = if y == h-1 then y else y+1
								   in
								   	   nextZigzagIndex x1 y1 d1
					
	
	toZigzag :: block a -> [a]
	toZigzag blk = map (\i -> xs !! i) $ zigzagScan blk
		where
			xs = toRaster blk
	
	-- zigzagScanInverse :: block a -> [Int]
	-- fromZigzag :: [a] -> block a
	-- fromZigzag xs = fromRaster $ map (\i -> xs !! i) $ zigzagScanInverse 



	

class (Block block) => SquareBlock block where
	


class (SquareBlock block) => Power2Block block where
	toMorton :: block a -> [a]
	-- toMorton blk = map (\i -> xs !! i) kMortonScan
	-- 	where
	-- 		xs = toRaster blk
	
	fromMorton :: [a] -> block a
	-- fromMorton xs = fromRaster $ map (\i -> xs !! i) kMortonScan
	
{-
morton :: Int -> [Int]
morton 0 = [0]
morton n = concat [submorton, map (+subsize) submorton, map (+2*subsize) submorton, map (+3*subsize) submorton]
	where
		subsize = 2^(2*(n-1))
		submorton = morton (n-1)

0,


0, 1,
2, 3,


0,	1,		2,	3,
4,	5,		6,	7,

8,	9,		10,	11,
12,	13,		14,	15,


0,	1,		2,	3,		4,	5,		6,	7,
8,	9,		10,	11,		12,	13,		14,	15,

16,	17,		18,	19,		20,	21,		22,	23,
24,	25,		26,	27,		28,	29,		30,	31,



0,

0,	1,	2,	3,

0,	1,	4,	5,
2,	3, 	6,	7,
8,	9,	12,	13,
10,	11, 14,	15,






0,1,4,5, 2,3,6,7, 8,9,12,13, 10,11,14,15

0,		 1,		  2,		 3,

0,1,2,3, 4,5,6,7, 8,9,10,11, 12,13,14,15
-}



