-- Hs264.NAL

module Hs264.Parsing.NAL where

import Control.Monad
import Data.Bits
import qualified Data.Bitstream.Lazy as BTL
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Debug.Trace
import System.IO

import Hs264.Context as CTX
import Hs264.Parsing.ByteStream
import Hs264.Parsing.NAL.SVCExtensions
import Hs264.Parsing.NAL.MVCExtensions
import Hs264.Parsing.SyntaxElement



btFromBs = BTL.fromByteString :: BSL.ByteString -> BitstreamBE


data NalUnitType = KNalUnitType0_Unspecified |
				   KNalUnitTypeSliceLayerNonIdrRbsp |
				   KNalUnitTypeSliceDataARbsp |
				   KNalUnitTypeSliceDataBRbsp |
				   KNalUnitTypeSliceDataCRbsp |
				   KNalUnitTypeSliceLayerIdrRbsp |
				   KNalUnitTypeSeiRbsp |
				   KNalUnitTypeSpsRbsp |
				   KNalUnitTypePpsRbsp |
				   KNalUnitTypeAccessUnitDelimiterRbsp |
				   KNalUnitTypeEndOfSequenceRbsp |
				   KNalUnitTypeEndOfStreamRbsp |
				   KNalUnitTypeFillerDataRbsp |
				   KNalUnitTypeSpsExtensionRbsp |
				   KNalUnitTypePrefixRbsp |
				   KNalUnitTypeSubsetSpsRbsp |
				   KNalUnitType16_Reserved |
				   KNalUnitType17_Reserved |
				   KNalUnitType18_Reserved |
				   KNalUnitTypeSliceLayerAuxRbsp |
				   KNalUnitTypeSliceExtensionRbsp |
				   KNalUnitTypeSliceDepthRbsp |
				   KNalUnitType22_Reserved |
				   KNalUnitType23_Reserved |
				   KNalUnitType24_Unspecified |
				   KNalUnitType25_Unspecified |
				   KNalUnitType26_Unspecified |
				   KNalUnitType27_Unspecified |
				   KNalUnitType28_Unspecified |
				   KNalUnitType29_Unspecified |
				   KNalUnitType30_Unspecified |
				   KNalUnitType31_Unspecified
				   deriving (Eq, Ord, Show, Read, Enum, Bounded)


data NalUnit = NalUnit { nalUnitType :: NalUnitType,
						 nalRefIdc :: Int,
						 nalSvcExtensionFlag :: Bool,
						 nalSvcHeader :: SvcHeader,
						 nalMvcHeader :: MvcHeader,
						 nalRbspBytes :: BSL.ByteString } deriving (Eq)

instance Show NalUnit where
	show nal = "NAL {" ++ show (nalUnitType nal) ++ ": refIdc=" ++ show (nalRefIdc nal) ++ ", rbsp=" ++ show (BSL.length $ nalRbspBytes nal) ++ "}"

emptyNal :: NalUnit
emptyNal = NalUnit { nalUnitType = KNalUnitType0_Unspecified,
					 nalRefIdc = 0,
					 nalSvcExtensionFlag = False,
					 nalSvcHeader = emptySvc,
	 				 nalMvcHeader = emptyMvc,
					 nalRbspBytes = BSL.empty }


readH264ByteStream :: FilePath -> IO String
readH264ByteStream fp = do
	bs <- BSL.readFile fp
	let mctx = decodeH264ByteStream bs CTX.empty
	if isJust mctx then
		return "cool"
	else
		return "error"


decodeH264ByteStream :: BSL.ByteString -> H264Context -> Maybe H264Context
decodeH264ByteStream bs ctx =
	getNextNalUnitBytes bs >>= \result -> 
	let
		(nalBytes, unparsedBytes) = result
	in
		do
			nal <- parseNalUnitBytes nalBytes
			ctx' <- decodeNalUnit nal ctx
			decodeH264ByteStream unparsedBytes ctx'


decodeNalUnit :: NalUnit -> H264Context -> Maybe H264Context
decodeNalUnit nal ctx | trace (show nal) False = undefined
decodeNalUnit nal ctx = Just ctx


unescapeRbsp :: BSL.ByteString -> BSL.ByteString
unescapeRbsp bs
	| not (isJust maybeIndex0) = bs
	| otherwise = BSL.append bsNoEmu bsSuffix
	where
		maybeIndex0 = BSL.elemIndex 0 bs
		index0 = fromJust maybeIndex0
		(bsNoEmu, bs0) = BSL.splitAt index0 bs
		bsSuffix = if kEmulationPrefix3 `BSL.isPrefixOf` bs0 then
					   BSL.cons' 0 $ BSL.cons' 0 $ unescapeRbsp $ BSL.drop 3 bs0
				   else
					   BSL.cons' 0 $ unescapeRbsp $ BSL.tail bs0
		kEmulationPrefix3 = BSL.pack [0,0,3]


-- spec 7.4.1
synelNUForbiddenZeroBit = parseAndValidateSynel (SynelTypeFn 1) (==0)
synelNUNalRefIdc = parseSynel (SynelTypeUn 2)
synelNUNalUnitType = parseSynel (SynelTypeUn 5)
synelNUSvcExtensionFlag = parseSynel (SynelTypeUn 1)



-- spec 7.3.1
parseNalUnitBytes :: BSL.ByteString -> Maybe NalUnit
parseNalUnitBytes bs =
	Just (btFromBs bs, []) >>=
	synelNUForbiddenZeroBit >>=
	synelNUNalRefIdc >>=
	synelNUNalUnitType >>= \(bt, vs) ->
	let
		refIdc = head $ tail vs
		nalType = (toEnum :: Int -> NalUnitType) $ last vs
		hasExtHeader = nalType == KNalUnitTypePrefixRbsp || 
					   nalType == KNalUnitTypeSliceExtensionRbsp ||
					   nalType == KNalUnitTypeSliceDepthRbsp
		headerSize = if hasExtHeader then 4 else 1
		nal = emptyNal { nalUnitType = nalType,
						 nalRefIdc = refIdc,
						 nalRbspBytes = unescapeRbsp $ BSL.drop headerSize bs }
	in
		if hasExtHeader then
			synelNUSvcExtensionFlag (bt, []) >>= \(xbt, xvs) ->
			let
				svcExtensionFlag = head xvs /= 0
			in
				if svcExtensionFlag then
					parseNalUnitHeaderSvcExtension xbt >>= \svc ->
					return nal { nalSvcHeader = svc }
				else
					parseNalUnitHeaderMvcExtension xbt >>= \mvc ->
					return nal { nalMvcHeader = mvc }
		else
			return nal
		

