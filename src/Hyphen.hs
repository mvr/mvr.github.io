{-# OPTIONS_GHC -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns
    -Wredundant-constraints #-}
{-# LANGUAGE OverloadedStrings, PartialTypeSignatures #-}

module Hyphen
    ( filterHyphen
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Builder hiding (Example)
import Text.Pandoc.Walk

replacements :: [(Text, Text)]
replacements = [("-categor", "‑categor"),
                ("-topos", "‑topos"),
                ("-morphism", "‑morphism")
               ]

fixText :: Text -> Text
fixText t = foldl (\t' (before, after) -> T.replace before after t') t replacements

fixInline :: Inline -> Inline
fixInline (Str xs) = Str $ fixText xs
fixInline x = x

filterHyphen :: Pandoc -> Pandoc
filterHyphen = walk fixInline
