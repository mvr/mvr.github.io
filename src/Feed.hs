{-# LANGUAGE OverloadedStrings #-}

module Feed
  ( sanitizeFeedItem
  , customRenderAtom
  ) where

import           Data.Maybe                     ( mapMaybe )
import qualified Data.Text                      as T
import           Hakyll
import           Hakyll.Web.Template           ( Template )
import           Text.Pandoc                    ( Pandoc(..) )
import           Text.Pandoc.Class              ( runIOorExplode )
import           Text.Pandoc.Definition
import           Text.Pandoc.Options
import           Text.Pandoc.Readers.HTML       ( readHtml )
import           Text.Pandoc.Walk               ( walk )
import           Text.Pandoc.Writers.HTML       ( writeHtml5String )

-- | Run a second Pandoc pass over a feed item, stripping interactive blocks
--   and trimming the content to the teaser before the <!--more--> marker.
sanitizeFeedItem :: Item String -> Compiler (Item String)
sanitizeFeedItem item = do
  cleaned <- unsafeCompiler . runIOorExplode $ do
    doc <- readHtml defaultHakyllReaderOptions (T.pack (itemBody item))
    let doc' = simplifyPandoc doc
    html <- writeHtml5String defaultHakyllWriterOptions doc'
    pure (T.unpack (T.strip html))
  pure $ itemSetBody cleaned item

simplifyPandoc :: Pandoc -> Pandoc
simplifyPandoc (Pandoc meta blocks) = Pandoc meta (go blocks)
  where
    go [] = []
    go (blk:rest) =
      case pruneBlock blk of
        Nothing -> go rest
        Just pruned
          | isMore pruned -> []
          | otherwise     -> pruned : go rest

    isMore (RawBlock "html" t) = "<!--more-->" `T.isInfixOf` T.toLower t
    isMore _                   = False

pruneBlock :: Block -> Maybe Block
pruneBlock (RawBlock "html" t)
  | isScript t = Nothing
pruneBlock (Div (_, classes, _) _)
  | any (== "lifeviewer") classes = Nothing
pruneBlock (Div attr blocks) =
  let inner = mapMaybe pruneBlock blocks
  in case inner of
       [] -> Nothing
       xs -> Just $ Div attr xs
pruneBlock block = Just (walk pruneInline block)

pruneInline :: Inline -> Inline
pruneInline (RawInline "html" t)
  | isScript t = Str ""
pruneInline inline = inline

isScript :: T.Text -> Bool
isScript = T.isPrefixOf "<script" . T.dropWhile (<= ' ') . T.toLower

customRenderAtom :: FeedConfiguration -> Context String -> [Item String] -> Compiler (Item String)
customRenderAtom config context items = do
  atomTemplate     <- loadBody "templates/atom.xml"
  atomItemTemplate <- loadBody "templates/atom-item.xml"
  renderAtomWithTemplates atomTemplate atomItemTemplate config context items
