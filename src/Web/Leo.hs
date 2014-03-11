{-# LANGUAGE OverloadedStrings #-}

module Web.Leo(query, Language) where

import Web.Leo.Types 

import Control.Exception
import Text.HTML.TagSoup
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as U

--    finally, we need to display the results on stdout
--    for that it would be nice to display as json or (c|t)sv

-- |'query' runs a translation query against the dict.leo.org webservice 
query :: String            -- ^ search term to translate
      -> Language          -- ^ language to translate to (e.g. 'En', 'Es' ...)
      -> Int               -- ^ maximum number of results to fetch
      -> IO [QueryResult]  -- ^ returns a list of QueryResult
query term l num = do
    result <- fetch term (l, De) num
    let categories = processRaw result
    return $ map toQueryResult categories

-- take a query string, some options and fetches a response from the webservice
fetch :: String -> Translation -> Int -> IO String
fetch q l num = do
    let req = defaultLeoOptions 
         { getTrans = l
         , getTerm = q
         , sectLenMax = num }
    result <- catch (simpleHttp $ show req)
                    (\e -> do let err = show (e :: HttpException)
                              fail "no connection")
    return $ U.toString $ L.toStrict result

-- takes a raw XML response, soupifies it and returns the result sections
processRaw :: String -> [[Tag String]]
processRaw s = matchT "section" $ parseTags s

-- turns a section and turns into a QueryResult
toQueryResult :: [Tag String] -> QueryResult
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

-- takes an entry and turns it into a Translation set
toTranslation :: [Tag String] -> (TEntry, TEntry)
toTranslation entry = (TEntry langL transL, TEntry langR transR)
    where sides = sideT entry

          langL  = parseLang $ head sides
          transL = map fromTagText $ textT $ head sides

          langR  = parseLang $ last sides
          transR = map fromTagText $ textT $ last sides

-- read lang attr and return Language
parseLang :: [Tag String] -> Language
parseLang s = read $ fromAttrib "lang" $ head s

-- match a partition/section with particular tag name
matchT :: String -> [Tag String] -> [[Tag String]]
matchT attr = partitions (~== TagOpen attr []) 

-- wrappers for <entry><side>...</side><side>...</side></entry>
entryT, sideT :: [Tag String] -> [[Tag String]]
entryT = matchT "entry"
sideT  = matchT "side"

-- extract tag contents
textT :: [Tag String] -> [Tag String]
textT s = filter (\n -> case n of 
                    TagText _   -> True
                    _           -> False) sliced 
    where sliced = takeWhile (not . isTagCloseName "words") rest 
          rest   = dropWhile (not . isTagOpenName "words") s
