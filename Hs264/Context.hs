-- Hs264.Context

module Hs264.Context where

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


addSps :: H264Context -> SequenceParameterSet -> H264Context
addSps ctx sps = ctx { ctxSpsStore = M.insert (spsSeqParameterSetId sps) sps (ctxSpsStore ctx) }

activeSps :: H264Context -> Maybe SequenceParameterSet
activeSps ctx = do
	spsId <- ctxActiveSpsId ctx
	M.lookup spsId (ctxSpsStore ctx)

setActiveSps :: Int -> H264Context -> Maybe H264Context
setActiveSps spsId ctx = do
	sps <- M.lookup spsId $ ctxSpsStore ctx
	return ctx { ctxActiveSpsId = Just spsId }

