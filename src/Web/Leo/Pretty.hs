{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Web.Leo.Pretty where

import Web.Leo.Types
import Text.PrettyPrint.ANSI.Leijen

instance Pretty Language where
    pretty De      = string "German"
    pretty En      = string "English"
    pretty Fr      = string "French"
    pretty Es      = string "Spanish"
    pretty It      = string "Italian" 
    pretty Ch      = string "Chinese" 
    pretty Ru      = string "Russian" 
    pretty Pt      = string "Portugese" 
    pretty Pl      = string "Polish"
    pretty Unknown = string "Unknown"

instance Pretty String where
    pretty     s  = text s
    prettyList ss = foldr (\n p -> pretty n <+> pretty p) empty ss

instance Pretty TEntry where
    pretty e = (pretty $ getResult e)

instance Pretty (TEntry,TEntry) where
    pretty (l,r) = fill 10 leftE <+> rightE 
        where leftE  = pretty $ getResult l
              rightE = pretty $ getResult r
    prettyList xs = foldr (\n p -> pretty n <+> fill 40 empty <+> pretty p) empty xs

instance Pretty QueryResult where
    pretty (Nouns    xs) = underline (bold (text "Nouns:"))    <$> indent 4 (vsep (map pretty xs))
    pretty (Phrase   xs) = underline (bold (text "Phrases:"))  <$> indent 4 (vsep (map pretty xs))
    pretty (Verbs    xs) = underline (bold (text  "Verbs:"))   <$> indent 4 (vsep (map pretty xs))
    pretty (AdjAdvs  xs) = underline (bold (text "AdjAdvs:"))  <$> indent 4 (vsep (map pretty xs))
    pretty (Examples xs) = underline (bold (text "Examples:")) <$> indent 4 (vsep (map pretty xs))
    pretty None          = string "None"

    prettyList xs = foldr (\n p -> pretty n <$> pretty p) empty xs

