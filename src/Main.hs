module Main(main) where

import Text.Read(readMaybe)
import Web.Leo(Language, query)
import Web.Leo.Pretty
import System.Console.CmdTheLine
import Control.Applicative
import qualified Text.PrettyPrint.ANSI.Leijen as PP

searchTerm :: Term String
searchTerm =  required $ pos 0 Nothing posInfo { posName = "PATTERN" }

numResults :: Term Int
numResults =  value $ opt 16 $ optInfo ["num","n"]

toLang :: Term String
toLang = value $ opt "en" $ optInfo ["language", "l"]

termInfo :: TermInfo
termInfo = defTI { termName = "leo", version = "1.0" }

runQuery :: Term (IO ())
runQuery = queryWithOptions <$> searchTerm <*> toLang <*> numResults

queryWithOptions :: String -> String -> Int -> IO () 
queryWithOptions term lang num = do
    let trans = readMaybe lang :: Maybe Language
    case trans of
        Just l -> do
            results <- query term l num
            PP.putDoc $ PP.pretty $ results
        _ -> fail "invalid language(s)"

main :: IO ()
main = run (runQuery, termInfo)

