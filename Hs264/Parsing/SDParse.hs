-- Hs264.Parsing.SDParse

module Hs264.Parsing.SDParse where

import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.Trans.State
import Data.Bits
import qualified Data.Map.Strict as M
import qualified Hs264.Data.SparseArray as SA
import Hs264.Parsing.BitParse
import qualified Hs264.Parsing.SynelDictionary as SD
import Hs264.Parsing.SyntaxElement

------------------------------------------------------------------------------
-- Syntax element parsing
------------------------------------------------------------------------------

-- type SDParseError = String
-- 
data SDParseState =
    SDParseState {
        sdpsBitPS :: BitParseState,
        sdpsDict :: SD.SynelDictionary
    } deriving (Show)


--type BitParse a = StateT BitParseState (EitherT BitParseError Identity) a
type SDParse a = StateT SD.SynelDictionary BitParse a
--type SDParse a = StateT SynelDictionary (StateT BitParseState (EitherT BitParseError Identity)) a

runSDParse :: SDParse a
           -> SD.SynelDictionary
           -> BitParseState
           -> Either BitParseError ((a, SD.SynelDictionary), BitParseState)
runSDParse p sd bps = runBitParse (runStateT p sd) bps

getSDState :: SDParse SDParseState
getSDState = do
    sd <- get
    bps <- lift $ get
    return $ SDParseState bps sd

putSDState :: SDParseState -> SDParse ()
putSDState (SDParseState{sdpsBitPS = bps, sdpsDict = sd}) = do
    put sd
    lift $ put bps



type SDSynelParse = SDParse Int


parse :: Synel -> SDSynelParse
parse = parseWith SD.setScalar

parseA :: Synel -> SDSynelParse
parseA = parseWith (\sd syn v -> SD.appendToArray sd syn [v])

parseSA :: Synel -> SA.SparseIndex -> SDSynelParse
parseSA synel ix = parseWith (\sd syn v -> SD.addToSparseArray sd syn ix v) synel


parseWith :: (SD.SynelDictionary -> Synel -> Int -> SD.SynelDictionary)
          -> Synel
          -> SDSynelParse
parseWith sdfun syn = do
    sdps@(SDParseState{sdpsBitPS = bps, sdpsDict = sd}) <- getSDState

    let bpResult = runBitParse (parseSynel syn) bps
    case bpResult of
        Left err -> StateT $ \_ -> (StateT $ \_ -> (EitherT $ return $ Left err))
        Right (value, bps') -> do
            let sd' = sdfun sd syn value
                sdps' = sdps {
                    sdpsBitPS = bps',
                    sdpsDict = sd'
                }
--            putSDState sdps'
--            StateT $ \_ -> (StateT $ \_ -> (EitherT $ return $ Right ((value, sd'), bps')))
            return value


parseForEach :: [a] -> (a -> SDSynelParse) -> SDParse ()
parseForEach [] _ = return ()
parseForEach (v:vs) f = do
    f v
    parseForEach vs f


parseWhenSD :: (SD.SynelDictionary -> Bool) -> SDSynelParse -> SDParse ()
parseWhenSD p doIfTrue = do
    SDParseState{sdpsDict = sd} <- getSDState
    when (p sd) $ do
        doIfTrue
        return ()


parseWhileSD :: (SD.SynelDictionary -> Bool) -> SDSynelParse -> SDParse ()
parseWhileSD p doWhileTrue = do
    sdps@(SDParseState{sdpsDict=sd}) <- getSDState
    when (p sd) $ do
        doWhileTrue
        parseWhileSD p doWhileTrue


parseWhen :: (SDParseState -> Bool) -> SDSynelParse -> SDParse ()
parseWhen p doIfTrue = do
    sdps <- getSDState
    when (p sdps) $ do
        doIfTrue
        return ()


parseWhile :: (SDParseState -> Bool) -> SDSynelParse -> SDParse ()
parseWhile p doWhileTrue = do
    sdps <- getSDState
    when (p sdps) $ do
        doWhileTrue
        parseWhile p doWhileTrue
    