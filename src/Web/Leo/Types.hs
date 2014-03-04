{-# LANGUAGE OverloadedStrings #-}

module Web.Leo.Types where

import qualified Data.ByteString.Char8 as CBS
import Data.Default(Default, def)
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

data LeoOptions = LeoOptions {
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

instance Default LeoOptions where
    def = defaultLeoOptions

type Translation = (Language,Language)

toString :: Translation -> String
toString s = show (fst s) ++ show (snd s)

-- | A Tanslation always has a language and a value, the translation
data TEntry = TEntry { 
        getLang   :: Language, 
        getResult :: [CBS.ByteString] 
    }
    deriving (Show, Eq)

data QueryResult = Nouns    [(TEntry,TEntry)] 
                 -- ^ Nouns    constructor is a list of translation tuples
                 | Phrase    [(TEntry,TEntry)] 
                 -- ^ Phras    constructor is a list of translation tuples
                 | Praep    [(TEntry,TEntry)] 
                 -- ^ Praepositions constructor is a list of translation tuples
                 | Verbs    [(TEntry,TEntry)] 
                 -- ^ Verbs    constructor is a list of translation tuples 
                 | AdjAdvs  [(TEntry,TEntry)] 
                 -- ^ AdjAdvs  constructor is a list of translation tuples 
                 | Examples [(TEntry,TEntry)] 
                 -- ^ Examples constructor is a list of translation tuples
                 | None
                 -- ^ None for queries with no result
                 deriving (Show)

data OutFormat = JSON | TSV | CSV
    deriving (Show, Eq)

defaultLeoOptions :: LeoOptions
defaultLeoOptions = LeoOptions { 
        getUrl     = "http://dict.leo.org/dictQuery/m-vocab",
        getTrans   = (En,De),
        getTerm    = "",
        searchLoc  = 0,
        sectLenMax = 16
    }
