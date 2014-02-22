module Web.Leo(
    Language
    , query
    ) where

import Text.HTML.TagSoup
import Network.HTTP

-- first,   we need to query leo and fetch the page
--    for that we need to have an option for queried language
-- next,    we need to parse the html and extract the tables containing the results
-- finally, we need to display the results on stdout
--    for that it would be nice to display as json or (c|t)sv

data Language = En --lish
              | Fr --ench
              | Sp --anish
              | It --alian 
              | Ch --inese 
              | Ru --ussian 
              | Pt -- Portugese 
              | Pl -- Polish

query = undefined

fetch :: Language -> [String] -> IO (String)
fetch lang q = do
    let get = getRequest "http://"
    result <- simpleHTTP get
    getResponseBody result

process :: String -> [Tag String]
process raw = filterTables parsed
    where parsed = parseTags raw

filterTables :: [Tag String] -> [Tag String]
filterTables xs = table
    where openTag = (\t -> case t of 
                                (TagOpen "table" _) -> True
                                _                    -> False)
          closeTag = (\t -> case t of
                                (TagClose "table") -> True
                                _                  -> False)
          ys = dropWhile (not . openTag) xs
          table = takeWhile (not . closeTag) ys

