module Web.Leo.Types where

import Network.HTTP(urlEncode)

data Language = En | Fr | Es | It | Ch | Ru | Pt | Pl | De | Unknown
    deriving (Eq)

-- |For simple parsing of Language from XML we use the Read class.
instance Read Language where
    readsPrec _ value = 
        tryParse langs 
        where langs = [("en", En), ("fr", Fr), ("es", Es),
                       ("it", It), ("ch", Ch), ("ru", Ru),
                       ("pt", Pt), ("pl", Pl), ("de", De)]
              -- yes, I read RWH. it was ... eye-opening! :)
              tryParse [] = []
              tryParse ((attempt,result):xs) =
                if take (length attempt) value == attempt   
                    then [(result, drop (length attempt) value)]
                    else tryParse xs

instance Show Language where
    show En = "en"
    show Fr = "fr"
    show Es = "es"
    show It = "it"
    show Ch = "ch"
    show Ru = "ru"
    show Pt = "pt"
    show Pl = "pl"
    show De = "de"

data LeoOptions = 
    LeoOptions {
        getUrl     :: String,
        getTrans   :: Translation,
        getTerm    :: String,
        searchLoc  :: Int,
        sectLenMax :: Int
    } 
    deriving (Eq)

instance Show LeoOptions where
    show (LeoOptions url trans search dir len) = 
        concat [ url , "/", toString trans, "/query.xml",
                "?tolerMode=nof",
                 "&lp=", toString trans,
                 "&lang=de&rmWords=off&rmSearch=on&directN=0&resultOrder=basic",
                 "&search=", urlEncode search, 
                 "&searchLoc=", show dir, 
                 "&sectLenMax=", show len ]

        where toString s = show (fst s) ++ show (snd s)

type Translation = (Language,Language)

-- | A Tanslation always has a language and a value, the translation
data Entry = 
    Entry { 
        getLang   :: Language, 
        getResult :: [String] 
    }
    deriving (Show, Eq)
 
data QueryResult = 
    Nouns      [(Entry,Entry)] 
    | Phrase   [(Entry,Entry)] 
    | Praep    [(Entry,Entry)] 
    | Verbs    [(Entry,Entry)] 
    | AdjAdvs  [(Entry,Entry)] 
    | Examples [(Entry,Entry)] 
    | None
    deriving (Show)

data OutFormat = JSON | TSV | CSV
    deriving (Show, Eq)

defaultLeoOptions :: LeoOptions
defaultLeoOptions = 
    LeoOptions { 
        getUrl     = "http://dict.leo.org/dictQuery/m-vocab",
        getTrans   = (En,De),
        getTerm    = "",
        searchLoc  = 0,
        sectLenMax = 16
    }
