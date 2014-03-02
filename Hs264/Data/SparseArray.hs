-- Hs264.Data.SparseArray

module Hs264.Data.SparseArray where

import qualified Data.Map.Strict as M


type SparseIndex = [Int]
type SparseArray a = M.Map SparseIndex a

empty :: SparseArray a
empty = M.empty

singleton :: SparseIndex -> a -> SparseArray a
singleton ix v = M.singleton ix v

lookup :: SparseArray a -> SparseIndex -> Maybe a
lookup sa ix = M.lookup ix sa

insert :: SparseArray a -> SparseIndex -> a -> SparseArray a
insert sa ix v = M.insert ix v sa

delete :: SparseArray a -> SparseIndex -> SparseArray a
delete sa ix = M.delete ix sa

union :: SparseArray a -> SparseArray a -> SparseArray a
union sa1 sa2 = M.union sa1 sa2
