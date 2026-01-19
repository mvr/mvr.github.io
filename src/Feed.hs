{-# LANGUAGE OverloadedStrings #-}

module Feed
  ( simplifyFeedPandoc
  , customRenderAtom
  ) where

import qualified Data.Text              as T
import           Hakyll
import           Hakyll.Web.Template    (Template)
import           Text.Pandoc            (Pandoc (..))
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk       (walk)

simplifyFeedPandoc :: Pandoc -> Pandoc
simplifyFeedPandoc = walk pruneInline . walk pruneBlock
  where
    pruneBlock (RawBlock "html" t)
      | isScript t = RawBlock "html" ""
    pruneBlock (CodeBlock (_, classes, _) _)
      | any (`elem` classes) feedDiagramClasses = Para [Str "[diagram]"]
    pruneBlock block = block

    pruneInline (RawInline "html" t)
      | isScript t = Str ""
    pruneInline (Note _) = Str ""
    pruneInline inline = inline

    feedDiagramClasses :: [T.Text]
    feedDiagramClasses = ["lifeviewer", "tikzpicture", "tikzcd", "mathpar"]

isScript :: T.Text -> Bool
isScript = T.isPrefixOf "<script" . T.dropWhile (<= ' ') . T.toLower

customRenderAtom :: FeedConfiguration -> Context String -> [Item String] -> Compiler (Item String)
customRenderAtom config context items = do
  atomTemplate     <- loadBody "templates/atom.xml"
  atomItemTemplate <- loadBody "templates/atom-item.xml"
  renderAtomWithTemplates atomTemplate atomItemTemplate config context items
