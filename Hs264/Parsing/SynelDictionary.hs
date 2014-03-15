-- Hs264.Parsing.SynelDictionary

module Hs264.Parsing.SynelDictionary where

import Data.Bits
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

emptySd :: SynelDictionary
emptySd = M.empty

sdHasKey :: SynelDictionary -> Synel -> Bool
sdHasKey sd key = M.member key sd

sdHasKeys :: SynelDictionary -> [Synel] -> Bool
sdHasKeys sd ks = all (sdHasKey sd) ks


sdScalar :: SynelDictionary -> Synel -> Int
sdScalar sd key = scalarValue
	where
		(SVScalar scalarValue) = sd M.! key

sdArray :: SynelDictionary -> Synel -> [Int]
sdArray sd key = arrayValue
	where
		(SVArray arrayValue) = sd M.! key
		
sdSparseArray :: SynelDictionary -> Synel -> SA.SparseArray Int
sdSparseArray sd key = saValue
	where
		(SVSparseArray saValue) = sd M.! key


sdSetScalar :: SynelDictionary -> Synel -> Int -> SynelDictionary
sdSetScalar sd key value = M.insert key (SVScalar value) sd

sdSetArray :: SynelDictionary -> Synel -> [Int] -> SynelDictionary
sdSetArray sd key vs = M.insert key (SVArray vs) sd

sdAppendToArray :: SynelDictionary -> Synel -> [Int] -> SynelDictionary
sdAppendToArray sd key vs = M.insertWith updateArray key (SVArray vs) sd
	where
		updateArray :: SynelValue -> SynelValue -> SynelValue
		updateArray (SVArray newvs) (SVArray oldvs) = SVArray (oldvs ++ newvs)

sdSetSparseArray :: SynelDictionary -> Synel -> SA.SparseArray Int -> SynelDictionary
sdSetSparseArray sd key sa = M.insert key (SVSparseArray sa) sd

sdAddToSparseArray :: SynelDictionary -> Synel -> SA.SparseIndex -> Int -> SynelDictionary
sdAddToSparseArray sd key ix v = M.insertWith updateSparseArray key (SVSparseArray $ SA.singleton ix v) sd
	where
		updateSparseArray :: SynelValue -> SynelValue -> SynelValue
		updateSparseArray (SVSparseArray newsa) (SVSparseArray oldsa) = SVSparseArray $ SA.union newsa oldsa


------------------------------------------------------------------------------
-- Syntax element parsing
------------------------------------------------------------------------------


type SDParseError = String

data SDParseState =
    SDParseState {
        sdpsBitPS :: BitParseState,
        sdpsDict :: SynelDictionary
    } deriving (Show)

newtype SDParse a =
    SDParse {
        runSDParse :: SDParseState -> Either SDParseError (a, SDParseState)
    }


getSDState :: SDParse SDParseState
getSDState = SDParse $ \s -> Right (s, s)

putSDState :: SDParseState -> SDParse ()
putSDState s = SDParse $ \_ -> Right ((), s)

failSDParse :: String -> SDParse a
failSDParse err = SDParse $ \s -> Left $ err ++ " SD:" ++ show (sdpsDict s)


instance Monad SDParse where
    return x = SDParse $ \s -> Right (x, s)
    fail = failSDParse
    parse >>= continuation = SDParse bareParse
        where
            bareParse ps =
                case runSDParse parse ps of
                    Left err -> Left err
                    Right (x, ps') -> runSDParse (continuation x) ps'


type SynelDictionaryParse = SDParse SDParseState


parse :: Synel -> SynelDictionaryParse
parse = parseWith sdSetScalar

parseA :: Synel -> SynelDictionaryParse
parseA = parseWith (\sd syn v -> sdAppendToArray sd syn [v])

parseSA :: Synel -> SA.SparseIndex -> SynelDictionaryParse
parseSA synel ix = parseWith (\sd syn v -> sdAddToSparseArray sd syn ix v) synel


parseWith :: (SynelDictionary -> Synel -> Int -> SynelDictionary)
          -> Synel
          -> SynelDictionaryParse
parseWith sdfun syn =
    getSDState >>= \sdps ->
    (let
        bps = sdpsBitPS sdps
        bitParse = parseSynel syn
    in
        case runBitParse bitParse bps of
            Left err ->
                failSDParse $ show syn ++ ": " ++ err
            Right (value, bps') ->
                (let
                    dict' = sdfun (sdpsDict sdps) syn value
                    sdps' = sdps {
                        sdpsBitPS = bps',
                        sdpsDict = dict'
                    }
                in
                    putSDState sdps' >>
                    return sdps'
                )
    )


parseIfSD :: (SynelDictionary -> Bool) -> SynelDictionaryParse -> SynelDictionaryParse
parseIfSD p ifTrue =
    getSDState >>= \ps ->
    (if p (sdpsDict ps) then
        ifTrue
    else
        return ps
    )


parseForEach :: [a] -> (a -> SynelDictionaryParse) -> SynelDictionaryParse
parseForEach [] _ = getSDState
parseForEach vs f =
	f (head vs) >>
	parseForEach (tail vs) f


parseWhile :: (SDParseState -> Bool) -> SynelDictionaryParse -> SynelDictionaryParse
parseWhile p f =
    getSDState >>= \sdps ->
    (if p sdps then
        f >>
        parseWhile p f
    else
        return sdps
    )
    
