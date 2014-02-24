{- GeneralizedNewtypeDeriving  -}

module Web.Leo(
    Language
    , query
    , fetch
    , processRaw
    , format
    ) where

import Text.HTML.TagSoup
import Network.HTTP

-- sections (~== TagOpen "sectionlist" []) str
-- sections (~== TagOpen "section" []) str

-- first,   we need to query leo and fetch the page
--    for that we need to have an option for queried language
-- next,    we need to parse the html and extract the tables containing the results
-- finally, we need to display the results on stdout
--    for that it would be nice to display as json or (c|t)sv

data Language = En | Fr | Sp | It | Ch | Ru | Pt | Pl | De | Unknown
              deriving (Show, Eq)

-- |For simple parsing of Language from XML we use the Read class.
instance Read Language where
    readsPrec _ value = 
        tryParse langs 
        where langs = [("en", En), ("fr", Fr), ("sp", Sp),
                       ("it", It), ("ch", Ch), ("ru", Ru),
                       ("pt", Pt), ("pl", Pl), ("de", De)]
              tryParse [] = []
              tryParse ((attempt,result):xs) =
                if (take (length attempt) value) == attempt   
                    then [(result, drop (length attempt) value)]
                    else tryParse xs

-- | A Tanslation always has a language and a value, the translation
data Translation = Translation { 
        language    :: Language, 
        translation :: String 
    }
    deriving (Show, Eq)

data QueryResult = Nouns    [(Translation,Translation)] -- ^ Nouns    constructor is a list of translation tuples
                 | Verbs    [(Translation,Translation)] -- ^ Verbs    constructor is a list of translation tuples 
                 | AdjAdvs  [(Translation,Translation)] -- ^ AdjAdvs  constructor is a list of translation tuples 
                 | Examples [(Translation,Translation)] -- ^ Examples constructor is a list of translation tuples
                 | None
                 deriving (Show)

data OutFormat = JSON | TSV | CSV
    deriving (Show, Eq)

-- |'query' runs a translation query against the dict.leo.org webservice 
query :: String            -- ^ search term to translate
      -> Language          -- ^ language to translate to/from 
      -> IO([QueryResult]) -- ^ returns a list of QueryResult
query term l = do
    result <- fetch term l
    let categories = processRaw result
    return $ map toQueryResult categories


-- |'fetch' take a query string, some options and fetches a response from the webservice
fetch :: String  -- ^ the query string
      -> Language -- ^ the Language to query results for 
      -> IO (String) -- ^ the String response
fetch lang q = do
    let get = getRequest "http://dict.leo.org/dictQuery/m-vocab/ende/query.xml?tolerMode=nof&lp=ende&lang=de&rmWords=off&rmSearch=on&directN=0&search=update&searchLoc=0&resultOrder=basic&sectLenMax=16"
    result <- simpleHTTP get
    getResponseBody result


-- |'processRaw' takes a raw XML response, soupifies it and returns the result sections
processRaw :: String -> [[Tag String]]
processRaw = partitions (~== TagOpen "section" []) . parseTags


-- |'toQueryResult' turns a section and turns into a QueryResult
toQueryResult :: [Tag String] -> QueryResult
toQueryResult soup = let category = head soup in 
        case (fromAttrib "sctName" category) of
            "subst"   -> Nouns $ collectResults soup
            "verb"    -> Verbs $ collectResults soup
            "adjadv"  -> AdjAdvs $ collectResults soup
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


format :: OutFormat -> [QueryResult] -> String
format = undefined 
