{-# LANGUAGE OverloadedStrings #-}

module Web.Leo(query, Language) where

import Text.HTML.TagSoup hiding (parseTags)
import Text.HTML.TagSoup.Fast.Utf8Only
import Network.HTTP
import Web.Leo.Types 
import qualified Data.ByteString.Char8 as CBS

--    finally, we need to display the results on stdout
--    for that it would be nice to display as json or (c|t)sv

-- |'query' runs a translation query against the dict.leo.org webservice 
query :: String            -- ^ search term to translate
      -> Language          -- ^ language to translate to/from 
      -> Int
      -> IO [QueryResult] -- ^ returns a list of QueryResult
query term l num = do
    result <- fetch term (l, De) num
    let categories = processRaw $ CBS.pack result
    return $ map toQueryResult categories

-- |'fetch' take a query string, some options and fetches a response from the webservice
fetch :: String  -- ^ the query string
      -> Translation -- ^ the Language to query results for 
      -> Int
      -> IO String -- ^ the String response
fetch q l num = do
    -- -get = getRequest "?tolerMode=nof&lp=ende&lang=de&rmWords=off&rmSearch=on&directN=0&search=update&searchLoc=0&resultOrder=basic&sectLenMax=16"
    let req = defaultLeoOptions
        flam = req { getTrans = l, getTerm = q, sectLenMax = num }
        get = getRequest $ show flam 
    print $ show flam
    result <- simpleHTTP get
    getResponseBody result

-- |'processRaw' takes a raw XML response, soupifies it and returns the result sections
processRaw :: CBS.ByteString -> [[Tag CBS.ByteString]]
processRaw s = matchT "section" tags
    where tags = parseTags s

-- |'toQueryResult' turns a section and turns into a QueryResult
toQueryResult :: [Tag CBS.ByteString] -> QueryResult
toQueryResult soup = let category = head soup in 
        case fromAttrib "sctName" category of
            "subst"   -> Nouns    $ collectR soup
            "phrase"  -> Phrase   $ collectR soup
            "praep"   -> Praep    $ collectR soup
            "verb"    -> Verbs    $ collectR soup
            "adjadv"  -> AdjAdvs  $ collectR soup
            "example" -> Examples $ collectR soup
            _         -> None
        where collectR s = map toTranslation $ entryT s

-- |'toTranslation' takes an entry and turns it into a Translation set
toTranslation :: [Tag CBS.ByteString] -> (TEntry, TEntry)
toTranslation entry = (TEntry langL transL, TEntry langR transR)
    where sides = sideT entry

          langL  = parseLang $ head sides
          transL = map fromTagText $ textT $ head sides

          langR  = parseLang $ last sides
          transR = map fromTagText $ textT $ last sides


parseLang :: [Tag CBS.ByteString] -> Language
parseLang s = read $ fromAttrib "lang" $ fmap CBS.unpack $ head s

matchT :: String -> [Tag CBS.ByteString] -> [[Tag CBS.ByteString]]
matchT attr = partitions (~== TagOpen attr []) 

entryT, sideT :: [Tag CBS.ByteString] -> [[Tag CBS.ByteString]]
entryT = matchT "entry"
sideT  = matchT "side"

textT :: [Tag CBS.ByteString] -> [Tag CBS.ByteString]
textT s = filter (\n -> case n of 
                    TagText _   -> True
                    _           -> False) sliced 
    where sliced = takeWhile (not . isTagCloseName "words") rest 
          rest   = dropWhile (not . isTagOpenName "words") s
