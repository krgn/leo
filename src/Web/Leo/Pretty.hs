{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE OverlappingInstances   #-}

module Web.Leo.Pretty where

import Web.Leo.Types
import Data.List
import Text.PrettyPrint.ANSI.Leijen ((<$>))
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

instance P.Pretty QueryResult where
    pretty (Nouns    xs) = prettify "Nouns:"                (boxify xs)
    pretty (Phrase   xs) = prettify "Phases:"               (boxify xs)
    pretty (Verbs    xs) = prettify "Verbs:"                (boxify xs)
    pretty (AdjAdvs  xs) = prettify "Adjectives/Adverbs:"   (boxify xs)
    pretty (Examples xs) = prettify "Examples:"             (boxify xs)
    pretty None          = P.string "None"

    prettyList xs = foldr (\n p -> P.pretty n <$> P.pretty p) P.empty xs

prettify :: String -> B.Box -> P.Doc
prettify a b = 
    P.underline (P.bold (P.text a)) 
    <$> P.linebreak 
    <$>P.indent 4 (P.string $ B.render b)

boxify :: [(TEntry,TEntry)] -> B.Box
boxify xs = box
    where cols  = transpose $ map (\(l,r) -> [procR l, procR r]) xs
          box   = B.hsep 2 B.left $ map (B.vcat B.left . map B.text) cols
          procR s = intercalate ", " $ getResult s
