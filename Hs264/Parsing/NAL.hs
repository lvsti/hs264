-- Hs264.Parsing.NAL

module Hs264.Parsing.NAL where

import Control.Monad
import Data.Bits
import qualified Data.Bitstream.Lazy as BTL
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Debug.Trace
import System.IO

import Hs264.Parsing.ByteStream
import Hs264.Parsing.NAL.SVCExtensions
import Hs264.Parsing.NAL.MVCExtensions
import Hs264.Parsing.RBSP.PPS
import Hs264.Parsing.RBSP.SPS
import Hs264.Parsing.SyntaxElement
import Hs264.Types.Context as CTX
import Hs264.Types.PPS
import Hs264.Types.SPS


btFromBs :: BSL.ByteString -> BitstreamBE
btFromBs = BTL.fromByteString



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
decodeNalUnit nal ctx | trace ("decoding " ++ show nal) False = undefined
decodeNalUnit nal ctx =
	case nalUnitType nal of
		KNalUnitTypeSpsRbsp ->
			parseSequenceParameterSetRbsp ctx bt >>= \(bt1, sps1) ->
			return $ CTX.storeSps sps1 ctx
		
		KNalUnitTypePpsRbsp ->
			parsePictureParameterSetRbsp ctx bt >>= \(bt2, pps2) ->
			return $ CTX.storePps pps2 ctx
		
		_ ->
			return ctx
		
	where
		bt = btFromBs $ nalRbspBytes nal


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
synelForbiddenZeroBit = mkSynelV "forbidden_zero_bit" (SynelTypeFn 1) (==0)
synelNalRefIdc = mkSynel "nal_ref_idc" (SynelTypeUn 2)
synelNalUnitType = mkSynel "nal_unit_type" (SynelTypeUn 5)
synelSvcExtensionFlag = mkSynel "svc_extension_flag" (SynelTypeUn 1)

-- spec 7.3.1
parseNalUnitBytes :: BSL.ByteString -> Maybe NalUnit
parseNalUnitBytes bs =
	Just (btFromBs bs, emptySd) >>=
	parse synelForbiddenZeroBit >>=
	parse synelNalRefIdc >>=
	parse synelNalUnitType >>= \(bt1, sd1) ->
	let
		refIdc = sdScalar sd1 synelNalRefIdc
		nalType = (toEnum :: Int -> NalUnitType) $ sdScalar sd1 synelNalUnitType
		hasExtHeader = nalType == KNalUnitTypePrefixRbsp || 
					   nalType == KNalUnitTypeSliceExtensionRbsp ||
					   nalType == KNalUnitTypeSliceDepthRbsp
		headerSize = if hasExtHeader then 4 else 1
		nal = emptyNal { nalUnitType = nalType,
						 nalRefIdc = refIdc,
						 nalRbspBytes = unescapeRbsp $ BSL.drop headerSize bs }
	in
		if hasExtHeader then
			Just (bt1, sd1) >>=
			parse synelSvcExtensionFlag >>= \(bt11, sd11) ->
			let
				svcExtensionFlag = sdScalar sd11 synelSvcExtensionFlag /= 0
			in
				if svcExtensionFlag then
					parseNalUnitHeaderSvcExtension bt11 >>= \svc ->
					return nal { nalSvcExtensionFlag = True, nalSvcHeader = svc }
				else
					parseNalUnitHeaderMvcExtension bt11 >>= \mvc ->
					return nal { nalSvcExtensionFlag = False, nalMvcHeader = mvc }
		else
			return nal
		

