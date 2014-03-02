module Web.Leo.Types where

import Data.Default(Default, def)
import Network.HTTP(urlEncode)


data ResultOrder = Basic | NoOrder
    deriving Eq

instance Show ResultOrder where 
    show Basic = "basic"

instance Read ResultOrder where
    readsPrec _ value = if value == "basic"
                        then [(Basic,   "")]
                        else [(NoOrder, "")]

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

newtype LeoBool = LB (Bool)

instance Show LeoBool where
    show (LB a) | a  = "on"
                | not a = "off"

instance Eq LeoBool where
    LB x == LB y = x == y
    LB x /= LB y = x /= y

data LeoOptions = LeoOptions {
        url         :: String,
        tolerMode   :: String,
        lang        :: Language,
        rmWords     :: LeoBool,
        rmSearch    :: LeoBool,
        directN     :: Int,
        search      :: String,
        searchLoc   :: Int,
        resultOrder :: ResultOrder,
        sectLenMax  :: Int
    } 
    deriving (Eq)

instance Show LeoOptions where
    show (LeoOptions u t d e f g h i j k) = 
        concat [ u , "/", show d, "de/query.xml",
                "?tolerMode=", t, 
                 "&lp=", show d, "de",
                 "&lang=de",
                 "&rmWords=", show e, 
                 "&rmSearch=", show f, 
                 "&directN=", show g,
                 "&search=", urlEncode h, 
                 "&searchLoc=", show i, 
                 "&resultOrder=", show j,
                 "&sectLenMax=", show k ]

instance Default LeoOptions where
    def = defaultLeoOptions

-- | A Tanslation always has a language and a value, the translation
data Translation = Translation { 
        language    :: Language, 
        translation :: String 
    }
    deriving (Show, Eq)

data QueryResult = Nouns    [(Translation,Translation)] 
                 -- ^ Nouns    constructor is a list of translation tuples
                 | Phrase    [(Translation,Translation)] 
                 -- ^ Nouns    constructor is a list of translation tuples
                 | Verbs    [(Translation,Translation)] 
                 -- ^ Verbs    constructor is a list of translation tuples 
                 | AdjAdvs  [(Translation,Translation)] 
                 -- ^ AdjAdvs  constructor is a list of translation tuples 
                 | Examples [(Translation,Translation)] 
                 -- ^ Examples constructor is a list of translation tuples
                 | None
                 -- ^ None for queries with no result
                 deriving (Show)

data OutFormat = JSON | TSV | CSV
    deriving (Show, Eq)

defaultLeoOptions :: LeoOptions
defaultLeoOptions = LeoOptions { 
        url = "http://dict.leo.org/dictQuery/m-vocab",
        tolerMode = "nof",
        lang = En,
        rmWords = LB False,
        rmSearch = LB True,
        directN = 0,
        search = "",
        searchLoc = 1,
        resultOrder = Basic,
        sectLenMax  = 16
    }
