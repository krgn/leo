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

data Translation = Translation { lang :: Language, val :: String }
    deriving (Show, Eq)

data QueryResult = Nouns    [(Translation,Translation)]
                 | Verbs    [(Translation,Translation)] 
                 | AdjAdvs  [(Translation,Translation)] 
                 | Examples [(Translation,Translation)]
                 | None
                 deriving (Show)

data OutFormat = JSON | TSV | CSV
    deriving (Show, Eq)

query :: String -> Language -> IO([QueryResult])
query term lang = do
    result <- fetch term lang
    let categories = processRaw result
    return $ map toQueryResult categories

-- TODO: build the request from options (general options type? Data.Default?)
fetch :: String -> Language -> IO (String)
fetch lang q = do
    let get = getRequest "http://dict.leo.org/dictQuery/m-vocab/ende/query.xml?tolerMode=nof&lp=ende&lang=de&rmWords=off&rmSearch=on&directN=0&search=update&searchLoc=0&resultOrder=basic&sectLenMax=16"
    result <- simpleHTTP get
    getResponseBody result

processRaw :: String -> [[Tag String]]
processRaw  = sections
    where sections = partitions (~== TagOpen "section" []) . parseTags

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

-- process the "entry"
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
