module Web.Leo(query, Language) where

import Text.HTML.TagSoup
import Network.HTTP
import Web.Leo.Types 

--    finally, we need to display the results on stdout
--    for that it would be nice to display as json or (c|t)sv

-- |'query' runs a translation query against the dict.leo.org webservice 
query :: String            -- ^ search term to translate
      -> Language          -- ^ language to translate to/from 
      -> IO [QueryResult] -- ^ returns a list of QueryResult
query term l = do
    result <- fetch term l
    let categories = processRaw result
    return $ map toQueryResult categories

-- |'fetch' take a query string, some options and fetches a response from the webservice
fetch :: String  -- ^ the query string
      -> Language -- ^ the Language to query results for 
      -> IO String -- ^ the String response
fetch q l = do
    -- -get = getRequest "?tolerMode=nof&lp=ende&lang=de&rmWords=off&rmSearch=on&directN=0&search=update&searchLoc=0&resultOrder=basic&sectLenMax=16"
    let req = defaultLeoOptions
        flam = req { lang = l, search = q }
        get = getRequest $ show flam 
    result <- simpleHTTP get
    getResponseBody result

-- |'processRaw' takes a raw XML response, soupifies it and returns the result sections
processRaw :: String -> [[Tag String]]
processRaw = partitions (~== TagOpen "section" []) . parseTags

-- |'toQueryResult' turns a section and turns into a QueryResult
toQueryResult :: [Tag String] -> QueryResult
toQueryResult soup = let category = head soup in 
        case fromAttrib "sctName" category of
            "subst"   -> Nouns    $ collectResults soup
            "phrase"  -> Phrase   $ collectResults soup
            "verb"    -> Verbs    $ collectResults soup
            "adjadv"  -> AdjAdvs  $ collectResults soup
            "example" -> Examples $ collectResults soup
            _         -> None
        where collectResults s = map toTranslation 
                                    $ partitions (~== TagOpen "entry" []) s

-- |'toTranslation' takes an entry and turns it into a Translation set
toTranslation :: [Tag String] -> (Translation, Translation)
toTranslation entry = (Translation a astr, Translation b bstr)
    where sides = partitions (~== TagOpen "side" []) entry
          a     = read $ fromAttrib "lang" $ head $ head sides
          astr  = concat $ map (fromTagText . head)
                    $ partitions (~== TagText "" ) $ head sides
          b     = read $ fromAttrib "lang" $ head $ last sides
          bstr  = concat $ map (fromTagText . head) 
                    $ partitions (~== TagText "" ) $ last sides
