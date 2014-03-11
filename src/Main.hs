module Main(main) where

import Web.Leo(Language, query)
import Web.Leo.Pretty
import Web.Leo.Json

import Text.Read(readMaybe)
import System.Console.CmdTheLine
import Control.Applicative
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy  as L

searchTerm :: Term String
searchTerm =  required $ pos 0 Nothing posInfo { posName = "PATTERN" }

format :: Term String
format =  value $ opt "tsv" $ optInfo ["format","f"]

numResults :: Term Int
numResults =  value $ opt 16 $ optInfo ["num","n"]

toLang :: Term String
toLang = value $ opt "en" $ optInfo ["language", "l"]

termInfo :: TermInfo
termInfo = defTI { termName = "leo", version = "1.0" }

runQuery :: Term (IO ())
runQuery = queryWithOptions <$> searchTerm <*> toLang <*> numResults <*> format

queryWithOptions :: String -> String -> Int -> String -> IO () 
queryWithOptions term lang num f = do
    let trans = readMaybe lang :: Maybe Language
    case trans of
        Just l -> do
            results <- query term l num

            case f of 
                "tsv"  -> PP.putDoc $ PP.pretty $ results
                "json" -> C.putStr $ L.toStrict $ encode results
                _      -> fail "invalid output format"
        _ -> fail "invalid language"

main :: IO ()
main = run (runQuery, termInfo)

