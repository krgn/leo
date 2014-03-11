{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

-- only export instances
module Web.Leo.Pretty() where

import Web.Leo.Types
import Data.List

import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Text.PrettyPrint.Boxes as B

instance P.Pretty Language where
    pretty De      = P.string "German"
    pretty En      = P.string "English"
    pretty Fr      = P.string "French"
    pretty Es      = P.string "Spanish"
    pretty It      = P.string "Italian" 
    pretty Ch      = P.string "Chinese" 
    pretty Ru      = P.string "Russian" 
    pretty Pt      = P.string "Portugese" 
    pretty Pl      = P.string "Polish"
    pretty Unknown = P.string "Unknown"

instance P.Pretty [QueryResult] where
    pretty r = render . hhcat $ map (B.vcat B.left . map B.text) $ accColumns r
        where render = P.string . B.render 
              hhcat  = B.hsep 2 B.left

accColumns :: [QueryResult] -> [[String]]
accColumns []     = [ ["No results"] ]
accColumns (x:[]) = toColumns x
accColumns (x:xs) = foldr proc  (toColumns x) xs
    where proc qr acc = [ head acc ++ [""] ++ (head $ toColumns qr)
                        , last acc ++ [""] ++ (last $ toColumns qr) ] 

toColumns :: QueryResult -> [[String]]
toColumns n = transpire (resultHeader n) (fromResult n)

transpire :: [[String]] -> [(Entry,Entry)] -> [[String]]
transpire header xs  = transpose $ (header ++ body)
    where body = map (\(l,r) -> [processResult l, processResult r]) xs

processResult :: Entry -> String
processResult = intercalate ", " . getResult

fromResult :: QueryResult -> [(Entry, Entry)]
fromResult (Nouns    xs) = xs
fromResult (Verbs    xs) = xs
fromResult (Phrase   xs) = xs 
fromResult (Praep    xs) = xs 
fromResult (AdjAdvs  xs) = xs
fromResult (Examples xs) = xs
fromResult None          = []

resultHeader :: QueryResult -> [[String]]
resultHeader (Nouns    _) = (prettier "Nouns"              : [""]) : emptyL
resultHeader (Verbs    _) = (prettier "Verbs"              : [""]) : emptyL
resultHeader (Phrase   _) = (prettier "Phrases"            : [""]) : emptyL
resultHeader (Praep    _) = (prettier "Praepositions"      : [""]) : emptyL
resultHeader (AdjAdvs  _) = (prettier "Adjectives/Adverbs" : [""]) : emptyL
resultHeader (Examples _) = (prettier "Examples"           : [""]) : emptyL
resultHeader None         = (prettier "Unknown"            : [""]) : emptyL

prettier :: String -> String
prettier = show . P.underline . P.bold . P.text

emptyL :: [[String]]
emptyL = ["",""] : []
