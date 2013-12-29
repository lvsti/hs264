-- Hs264.NAL

module Hs264.Parsing.ByteStream where

import qualified Data.ByteString.Lazy as BSL
import Data.Int
import Data.Maybe


kPrefixOne3 = BSL.pack [0,0,1]
kPrefixOne4 = BSL.pack [0,0,0,1]
kPrefixZero3 = BSL.pack [0,0,0]


-- spec B.1.2
synLeadingZero8bits :: BSL.ByteString -> Maybe BSL.ByteString
synLeadingZero8bits bs
	| BSL.null bs = Nothing
	| otherwise = if BSL.head bs == 0 then Just $ BSL.tail bs else Nothing


synZeroByte :: BSL.ByteString -> Maybe BSL.ByteString
synZeroByte bs
	| BSL.null bs = Nothing
	| otherwise = if BSL.head bs == 0 then Just $ BSL.tail bs else Nothing


synStartCodePrefixOne3bytes :: BSL.ByteString -> Maybe BSL.ByteString
synStartCodePrefixOne3bytes bs
	| BSL.length bs < 3 = Nothing
	| otherwise = if kPrefixOne3 `BSL.isPrefixOf` bs then
			 		  Just $ BSL.drop 3 bs
				  else
					  Nothing


synTrailingZero8bits :: BSL.ByteString -> Maybe BSL.ByteString
synTrailingZero8bits bs
	| BSL.null bs = Nothing
	| otherwise = if BSL.head bs == 0 then Just $ BSL.tail bs else Nothing



parseLeadingZeroes :: BSL.ByteString -> Maybe BSL.ByteString
parseLeadingZeroes bs
	| BSL.length bs < 3 = Nothing
	| otherwise = if kPrefixOne3 `BSL.isPrefixOf` bs || kPrefixOne4 `BSL.isPrefixOf` bs then
					  if kPrefixOne3 `BSL.isPrefixOf` bs then
						  Just bs
					  else
						  synZeroByte bs
				  else
					  let
					  	  trimmedBs = synLeadingZero8bits bs
					  in
						  if isJust trimmedBs then
							  parseLeadingZeroes $ fromJust trimmedBs
						  else
							  Nothing


parseTrailingZeroes :: BSL.ByteString -> Maybe BSL.ByteString
parseTrailingZeroes bs
	| BSL.null bs = Just bs
	| otherwise = if kPrefixOne3 `BSL.isPrefixOf` bs || kPrefixOne4 `BSL.isPrefixOf` bs then
					  Just bs
				  else
					  let
					  	  trimmedBs = synTrailingZero8bits bs
					  in
						  if isJust trimmedBs then
							  parseTrailingZeroes $ fromJust trimmedBs
						  else
							  Nothing


calculateNalLength :: BSL.ByteString -> Int64
calculateNalLength bs
	| not $ isJust maybeIndex0 = BSL.length bs
	| otherwise = if kPrefixOne3 `BSL.isPrefixOf` bs0 || kPrefixZero3 `BSL.isPrefixOf` bs0 then
					  index0
				  else
					  index0 + 1 + calculateNalLength (BSL.tail bs0)
	where
		maybeIndex0 = BSL.elemIndex 0 bs
		index0 = fromJust maybeIndex0
		bs0 = BSL.drop index0 bs


getNextNalUnitBytes :: BSL.ByteString -> Maybe (BSL.ByteString, BSL.ByteString)
getNextNalUnitBytes bs = 
	Just bs >>=
	parseLeadingZeroes >>=
	synStartCodePrefixOne3bytes >>= \nalbs ->
	let
		nalLength = calculateNalLength nalbs
		trailingBytes = BSL.drop nalLength nalbs
	in
		parseTrailingZeroes trailingBytes >>= \unpbs ->
		return (BSL.take nalLength nalbs, unpbs)


