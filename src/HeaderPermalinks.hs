{-# LANGUAGE OverloadedStrings #-}

module HeaderPermalinks
    ( addHeaderPermalinks
    ) where

import qualified Data.Text as T
import Text.Pandoc.Definition
import Text.Pandoc.Walk (walk)

addHeaderPermalinks :: Pandoc -> Pandoc
addHeaderPermalinks = walk addPermalink

addPermalink :: Block -> Block
addPermalink (Header level attr@(ident, _, _) inlines)
  | not (T.null ident) && not (hasPermalink inlines) =
      Header level attr (inlines <> [Space, permalink ident])
addPermalink block = block

permalink :: T.Text -> Inline
permalink ident =
  Link
    ("", ["heading-permalink"], [("aria-label", "Permalink to this section"), ("title", "Permalink")])
    [Str "§"]
    ("#" <> ident, "")

hasPermalink :: [Inline] -> Bool
hasPermalink = any isPermalink
  where
    isPermalink (Link (_, classes, _) _ _) = "heading-permalink" `elem` classes
    isPermalink _ = False
