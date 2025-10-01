{-# LANGUAGE OverloadedStrings #-}

module Feed
  ( sanitizeFeedItem
  , customRenderAtom
  ) where

import           Data.Maybe             (mapMaybe)
import qualified Data.Text              as T
import           Hakyll
import           Hakyll.Web.Template    (Template)
import           Text.Pandoc            (Pandoc (..))
import           Text.Pandoc.Class      (runIOorExplode)
import           Text.Pandoc.Definition
import           Text.Pandoc.Options
import           Text.Pandoc.Readers.HTML (readHtml)
import           Text.Pandoc.Walk       (walk)
import           Text.Pandoc.Writers.HTML (writeHtml5String)

-- | Run a second Pandoc pass over a feed item, stripping interactive blocks
--   and trimming the content to the teaser before the <!--more--> marker.
sanitizeFeedItem :: Item String -> Compiler (Item String)
sanitizeFeedItem item = do
  let raw = T.pack (itemBody item)
      teaserHtml = fst (T.breakOn "<!--more-->" raw)
  cleaned <- unsafeCompiler . runIOorExplode $ do
    doc <- readHtml defaultHakyllReaderOptions teaserHtml
    let doc' = simplifyPandoc doc
    html <- writeHtml5String defaultHakyllWriterOptions doc'
    pure (T.unpack (T.strip html))
  pure $ itemSetBody cleaned item

simplifyPandoc :: Pandoc -> Pandoc
simplifyPandoc (Pandoc meta blocks) = Pandoc meta (process blocks)
  where
    process [] = []
    process (blk:rest) =
      case pruneBlock blk of
        Nothing -> process rest
        Just pruned
          | isMore pruned -> []
          | otherwise     -> pruned : process rest

    isMore (RawBlock "html" t) = "<!--more-->" `T.isInfixOf` T.toLower t
    isMore _                   = False

pruneBlock :: Block -> Maybe Block
pruneBlock (RawBlock "html" t)
  | isScript t = Nothing
  | isSideNoteHtml t = Nothing
pruneBlock (Div attr blocks)
  | hasClass "lifeviewer" attr = Just (Para [Str "[diagram]"])
  | isSideNoteAttr attr = Nothing
  | otherwise =
      let inner = mapMaybe pruneBlock blocks in
      case inner of
        [] -> Nothing
        xs -> Just $ Div attr xs
pruneBlock (Para inlines)
  | any isDiagramInline inlines = Just (Para [Str "[diagram]"])
pruneBlock (Plain inlines)
  | any isDiagramInline inlines = Just (Plain [Str "[diagram]"])
pruneBlock block = Just (walk pruneInline block)

pruneInline :: Inline -> Inline
pruneInline (RawInline "html" t)
  | isScript t = Str ""
  | isSideNoteHtml t = Str ""
pruneInline (Math InlineMath tex) = Code ("", [], []) (normalizeMath tex)
pruneInline (Math DisplayMath _) = Str "[math]"
pruneInline (Span attr inlines)
  | isSideNoteAttr attr = Str ""
  | hasClass "math" attr = Code ("", [], []) (inlineToText pruned)
  | hasClass "katex" attr = Code ("", [], []) (inlineToText pruned)
  | hasClass "katex-display" attr = Str "[diagram]"
  | hasClass "math-display" attr = Str "[diagram]"
  | otherwise = Span attr pruned
  where
    pruned = map pruneInline inlines
pruneInline (Image attr _ _)
  | isDiagramAttr attr = Str "[diagram]"
pruneInline inline = inline

isScript :: T.Text -> Bool
isScript = T.isPrefixOf "<script" . T.dropWhile (<= ' ') . T.toLower

hasClass :: T.Text -> Attr -> Bool
hasClass cls (_, classes, _) = cls `elem` classes

inlineToText :: [Inline] -> T.Text
inlineToText = T.concat . fmap inlineText
  where
    inlineText (Str s)      = s
    inlineText Space        = " "
    inlineText SoftBreak    = " "
    inlineText LineBreak    = " "
    inlineText (Code _ s)   = s
    inlineText (Math _ s)   = s
    inlineText (RawInline _ s) = s
    inlineText (Span _ xs)  = inlineToText xs
    inlineText (Emph xs)    = inlineToText xs
    inlineText (Strong xs)  = inlineToText xs
    inlineText (Link _ xs _) = inlineToText xs
    inlineText _            = ""

normalizeMath :: T.Text -> T.Text
normalizeMath = T.strip

isDiagramInline :: Inline -> Bool
isDiagramInline (Image attr _ _) = isDiagramAttr attr
isDiagramInline (Span attr _)    = any (`hasClass` attr) diagramClasses
isDiagramInline _                = False

isDiagramAttr :: Attr -> Bool
isDiagramAttr attr = any (`hasClass` attr) diagramClasses

diagramClasses :: [T.Text]
diagramClasses =
  [ "tikzpicture"
  , "tikzcd"
  , "mathpar"
  , "diagram"
  , "lifeviewer"
  , "katex-display"
  , "math-display"
  ]

isSideNoteAttr :: Attr -> Bool
isSideNoteAttr attr = any (`hasClass` attr) sideNoteClasses

sideNoteClasses :: [T.Text]
sideNoteClasses = ["sidenote", "sidenote-wrapper", "marginnote", "sidenote-number", "margin-toggle"]

isSideNoteHtml :: T.Text -> Bool
isSideNoteHtml t = any (`T.isInfixOf` lowerT) patterns
  where
    lowerT = T.toLower t
    patterns =
      [ "class=\"sidenote"
      , "class='sidenote"
      , "class=\"marginnote"
      , "class='marginnote"
      , "sidenote-wrapper"
      , "margin-toggle"
      ]

customRenderAtom :: FeedConfiguration -> Context String -> [Item String] -> Compiler (Item String)
customRenderAtom config context items = do
  atomTemplate     <- loadBody "templates/atom.xml"
  atomItemTemplate <- loadBody "templates/atom-item.xml"
  renderAtomWithTemplates atomTemplate atomItemTemplate config context items
