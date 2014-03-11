{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- only instances are exported
module Web.Leo.Json(encode) where

import Web.Leo.Types
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T

instance ToJSON Language where
    toJSON l = String $ T.pack $ show l

instance ToJSON Entry where
    toJSON (Entry l r) = object [ "language" .= l, "result" .= r ]

instance ToJSON (Entry,Entry) where
    toJSON (l, r) = object [ 
        (T.pack $ show $ getLang l) .= getResult l
        , (T.pack $ show $ getLang r) .= getResult r 
        ]
 
instance ToJSON [QueryResult] where
    toJSON results = object $ foldr processResult [] results
        where processResult :: QueryResult -> [Pair] -> [Pair]
              processResult (Nouns     ts) ps = ps ++ ["nouns"   .= ts]
              processResult (Verbs     ts) ps = ps ++ ["verbs"   .= ts]
              processResult (Phrase    ts) ps = ps ++ ["phrase"  .= ts]
              processResult (Praep     ts) ps = ps ++ ["praep"   .= ts]
              processResult (AdjAdvs   ts) ps = ps ++ ["adjadvs" .= ts]
              processResult (Examples  ts) ps = ps ++ ["exmples" .= ts]
              processResult None           ps = ps
