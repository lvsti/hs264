-- Hs264.Types

module Hs264.Types where

import Data.Word
import Data.Int
import Data.List

import Hs264.Block


type Sample = Word8
type Arithmetic = Int



data Block4x4 a = Block4x4 [a] deriving (Eq, Show)
data Block2x2 a = Block2x2 [a] deriving (Eq, Show)

instance Block Block4x4 where
	width _ = 4
	height _ = 4
	toRaster (Block4x4 xs) = xs
	fromRaster = Block4x4

instance SquareBlock Block4x4 where


kMortonScan4x4 = [0,1,4,5,2,3,6,7,8,9,12,13,10,11,14,15] :: [Int]

instance Power2Block Block4x4 where
	toMorton blk = map (\i -> xs !! i) kMortonScan4x4
		where
			xs = toRaster blk
	
	fromMorton xs = fromRaster $ map (\i -> xs !! i) kMortonScan4x4


instance Block Block2x2 where
	width _ = 2
	height _ = 2
	toRaster (Block2x2 xs) = xs
	fromRaster = Block2x2
	toZigzag = toRaster
--	fromZigzag = fromRaster

instance SquareBlock Block2x2 where

instance Power2Block Block2x2 where
	toMorton = toRaster
	fromMorton = fromRaster


type Sample4x4 = Block4x4 Sample
type Arithmetic2x2 = [Arithmetic]
type Arithmetic4x4 = Block4x4 Arithmetic



	-- 
	-- blk4x4ToRows xs = map (\i -> take 4 $ drop (4*i) xs) [0..3]
	-- 
	-- blk4x4ToColumns :: [a] -> [[a]]
	-- blk4x4ToColumns = transpose . blk4x4ToRows
	-- 
	-- blk4x4Raster :: [a] -> [a]
	-- blk4x4Raster = id
	-- 
	-- blk4x4Zigzag :: [a] -> [a]
	-- blk4x4Zigzag xs = map (\i -> xs !! i) zigzagScan
	-- 	where
	-- 		zigzagScan = [0,1,4,8,5,2,3,6,9,12,13,10,7,11,14,15]
	-- 
	-- blk4x4Morton :: [a] -> [a]
	-- blk4x4Morton xs = map (\i -> xs !! i) mortonScan
	-- 	where
	-- 		mortonScan = [0,1,4,5,2,3,6,7,8,9,12,13,10,11,14,15]
	-- 
	-- blk4x4FromRows :: [[a]] -> [a]
	-- blk4x4FromRows = concat
	-- 
	-- blk4x4FromColumns :: [[a]] -> [a]
	-- blk4x4FromColumns = blk4x4FromRows . transpose
	-- 