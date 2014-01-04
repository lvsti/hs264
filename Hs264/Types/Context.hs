-- Hs264.Types.Context

module Hs264.Types.Context where

import qualified Data.Map.Strict as M

import Hs264.Types.SPS


data H264Context =
	H264Context {
		ctxSpsStore :: M.Map Int SequenceParameterSet,
		ctxActiveSpsId :: Maybe Int
	} deriving (Eq, Show)


empty :: H264Context
empty =
	H264Context {
		ctxSpsStore = M.empty,
		ctxActiveSpsId = Nothing
	}


storeSps :: SequenceParameterSet -> H264Context -> H264Context
storeSps sps ctx = ctx { ctxSpsStore = M.insert (spsSeqParameterSetId sps) sps (ctxSpsStore ctx) }

lookupSps :: Int -> H264Context -> Maybe SequenceParameterSet
lookupSps spsId ctx = M.lookup spsId (ctxSpsStore ctx)

activeSps :: H264Context -> Maybe SequenceParameterSet
activeSps ctx = do
	spsId <- ctxActiveSpsId ctx
	lookupSps spsId ctx

setActiveSps :: Int -> H264Context -> Maybe H264Context
setActiveSps spsId ctx = do
	sps <- M.lookup spsId $ ctxSpsStore ctx
	return ctx { ctxActiveSpsId = Just spsId }

