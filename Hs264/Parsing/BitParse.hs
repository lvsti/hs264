-- Hs264.Parsing.BitParse

module Hs264.Parsing.BitParse where

import Control.Monad.Identity
import Control.Monad.Trans.Either
import Control.Monad.Trans.State
import qualified Data.Bitstream.Lazy as BTL


------------------------------------------------------------------------------
-- Bitstream parser
------------------------------------------------------------------------------

-- big endian bitstream
type BitstreamBE = BTL.Bitstream BTL.Right

type BitParseError = String

data BitParseState =
    BitParseState {
        bpsBits :: BitstreamBE,
        bpsOffset :: Int
    }

instance Show BitParseState where
    show bps = "<ofs:" ++ show (bpsOffset bps) ++ ">"


type BitParse = StateT BitParseState (EitherT BitParseError Identity)

runBitParse :: BitParse a -> BitParseState -> Either BitParseError (a, BitParseState)
runBitParse p bps = runIdentity $ runEitherT $ runStateT p bps


getBPState :: BitParse BitParseState
getBPState = get

putBPState :: BitParseState -> BitParse ()
putBPState = put

failBP :: String -> BitParse a
failBP err = StateT $ \bps -> EitherT $ return $ Left $ err ++ " " ++ show bps


