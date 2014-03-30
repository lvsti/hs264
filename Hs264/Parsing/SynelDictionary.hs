-- Hs264.Parsing.SynelDictionary

module Hs264.Parsing.SynelDictionary where

import qualified Data.Map.Strict as M
import qualified Hs264.Data.SparseArray as SA
import Hs264.Parsing.SyntaxElement


------------------------------------------------------------------------------
-- Syntax element dictionary
------------------------------------------------------------------------------

data SynelValue = SVScalar Int
                | SVArray [Int]
                | SVSparseArray (SA.SparseArray Int)
                deriving (Eq,Show)

type SynelDictionary = M.Map Synel SynelValue

empty :: SynelDictionary
empty = M.empty

hasKey :: SynelDictionary -> Synel -> Bool
hasKey sd key = M.member key sd

hasKeys :: SynelDictionary -> [Synel] -> Bool
hasKeys sd ks = all (hasKey sd) ks


scalar :: SynelDictionary -> Synel -> Int
scalar sd key = scalarValue
    where
        (SVScalar scalarValue) = sd M.! key

array :: SynelDictionary -> Synel -> [Int]
array sd key = arrayValue
    where
        (SVArray arrayValue) = sd M.! key

sparseArray :: SynelDictionary -> Synel -> SA.SparseArray Int
sparseArray sd key = saValue
    where
        (SVSparseArray saValue) = sd M.! key


setScalar :: SynelDictionary -> Synel -> Int -> SynelDictionary
setScalar sd key value = M.insert key (SVScalar value) sd

setArray :: SynelDictionary -> Synel -> [Int] -> SynelDictionary
setArray sd key vs = M.insert key (SVArray vs) sd

appendToArray :: SynelDictionary -> Synel -> [Int] -> SynelDictionary
appendToArray sd key vs = M.insertWith updateArray key (SVArray vs) sd
    where
        updateArray :: SynelValue -> SynelValue -> SynelValue
        updateArray (SVArray newvs) (SVArray oldvs) = SVArray (oldvs ++ newvs)

setSparseArray :: SynelDictionary -> Synel -> SA.SparseArray Int -> SynelDictionary
setSparseArray sd key sa = M.insert key (SVSparseArray sa) sd

addToSparseArray :: SynelDictionary -> Synel -> SA.SparseIndex -> Int -> SynelDictionary
addToSparseArray sd key ix v = M.insertWith updateSparseArray key (SVSparseArray $ SA.singleton ix v) sd
    where
        updateSparseArray :: SynelValue -> SynelValue -> SynelValue
        updateSparseArray (SVSparseArray newsa) (SVSparseArray oldsa) = SVSparseArray $ SA.union newsa oldsa


