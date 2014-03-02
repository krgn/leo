module Main(main) where

import Text.Read(readMaybe)
import Web.Leo(Language, query)
import System.Console.CmdTheLine
import Control.Applicative

searchTerm :: Term String
searchTerm =  required $ pos 0 Nothing posInfo { posName = "PATTERN" }

numResults :: Term Int
numResults =  value $ opt 16 $ optInfo ["num","n"]

language :: Term String
language = value $ opt "en" $ optInfo ["language", "l"]

termInfo :: TermInfo
termInfo = defTI { termName = "leo", version = "1.0" }

runQuery :: Term (IO ())
runQuery = queryWithOptions <$> searchTerm <*> language <*> numResults

queryWithOptions :: String -> String -> Int -> IO () 
queryWithOptions term lang num = do
    case readMaybe lang :: Maybe Language of
        Just l -> do
            results <- query term l
            putStrLn $ show results
        Nothing -> fail "invalid language"

main :: IO ()
main = run (runQuery, termInfo)
