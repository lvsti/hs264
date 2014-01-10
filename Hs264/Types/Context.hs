-- Hs264.Types.Context

module Hs264.Types.Context where

import qualified Data.Map.Strict as M

import Hs264.Types.SPS


data H264Context =
	H264Context {
		ctxSpsStore :: M.Map Int SequenceParameterSet,
		ctxPpsStore :: M.Map Int PictureParameterSet
	} deriving (Show)


empty :: H264Context
empty =
	H264Context {
		ctxSpsStore = M.empty,
		ctxPpsStore = M.empty
	}


storeSps :: SequenceParameterSet -> H264Context -> H264Context
storeSps sps ctx = ctx { ctxSpsStore = M.insert (spsSeqParameterSetId sps) sps (ctxSpsStore ctx) }

lookupSps :: Int -> H264Context -> Maybe SequenceParameterSet
lookupSps spsId ctx = M.lookup spsId (ctxSpsStore ctx)

storePps :: PictureParameterSet -> H264Context -> H264Context
storePps pps ctx = ctx { ctxPpsStore = M.insert (ppsPicParameterSetId pps) pps (ctxPpsStore ctx) }

lookupPps :: Int -> H264Context -> Maybe PictureParameterSet
lookupPps ppsId ctx = M.lookup ppsId (ctxPpsStore ctx)

