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
    pretty e = (bold $ pretty $ getLang e) <+> (pretty $ getResult e)

instance Pretty (TEntry,TEntry) where
    pretty (l,r) = fill 10 leftE <+> rightE 
        where leftE  = (bold $ pretty $ getLang l) <+> (pretty $ getResult l)
              rightE = (pretty $ getResult r) <+> (bold $ pretty $ getLang r)
    prettyList xs = foldr (\n p -> pretty n <$> pretty p) empty xs

instance Pretty QueryResult where
    pretty (Nouns    xs) = string "Nouns:"    <$> prettyList xs
    pretty (Phrase   xs) = string "Phrases:"  <$> prettyList xs
    pretty (Verbs    xs) = string "Verbs:"    <$> prettyList xs
    pretty (AdjAdvs  xs) = string "AdjAdvs:"  <$> prettyList xs
    pretty (Examples xs) = string "Examples:" <$> prettyList xs
    pretty None          = string "None"

    prettyList xs = foldr (\n p -> pretty n <$> pretty p) empty xs

