{-# LANGUAGE OverloadedStrings  #-}

module Main (main) where

import Control.Monad (liftM)
import Data.Monoid
import System.Environment (getArgs)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Text.Titlecase

import Hakyll
import Text.Pandoc.Options

main :: IO ()
main = do
  args <- getArgs
  case args of
    (h:_) -> run h
    []    -> run "blank"

perPage :: Int
perPage = 3

grouper :: (MonadMetadata m, MonadFail m) => [Identifier] -> m [[Identifier]]
grouper ids = (liftM (paginateEvery perPage) . sortRecentFirst) ids

makeId :: PageNumber -> Identifier
makeId pageNum = fromFilePath $ "page/" ++ (show pageNum) ++ ".html"

-- Much is stolen from https://github.com/jaspervdj/jaspervdj
run :: String -> IO ()
run action = hakyllWith config $ do
  let postsPattern = if action == "watch"
                     then "posts/*" -- .||. "inprogress/*"
                     else "posts/*"

  pag <- buildPaginateWith grouper postsPattern makeId
  tags <- buildTags postsPattern (fromCapture "tags/*.html")

  -- Compile posts
  match postsPattern $ do
    route   $ setExtension ".html"
    let ctx = postCtx tags -- <> field "tags" (\_ -> renderTagList tags)
    compile $ pandocMathCompiler
      >>= saveSnapshot "body"
      >>= loadAndApplyTemplate "templates/post.html" ctx
      >>= saveSnapshot "rendered"
      >>= loadAndApplyTemplate "templates/post-page.html" ctx
      >>= loadAndApplyTemplate "templates/default.html" ctx
      >>= relativizeUrls

  -- Post list
  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll postsPattern
      let ctx = constField "title" "All Posts" <>
                listField "posts" (postCtx tags) (return posts) <>
                defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" ctx
        >>= loadAndApplyTemplate "templates/post-list-page.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls



  -- Paginated pages
  paginateRules pag $ \pageNum pattern -> do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots pattern "body"
      let paginateCtx = paginateContext pag pageNum
          ctx =
            field "tags" (\_ -> renderTagList tags) <>
            constField "title" ("Page " ++ (show pageNum)) <>
            listField "posts" (postCtx tags) (return posts) <>
            paginateCtx <>
            defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/post-list.html" ctx
        >>= loadAndApplyTemplate "templates/post-list-page.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  -- Tags
  tagsRules tags $ \tag pattern -> do
    let title = "Posts Tagged ‘" ++ titlecase (fmap (\c -> if c == '-' then ' ' else c) tag) ++ "’"

    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots pattern "body"
      let ctx = constField "title" title <>
                listField "posts" (postCtx tags) (return posts) <>
                field "tags" (\_ -> renderTagList tags) <>
                defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/post-list.html" ctx
        >>= loadAndApplyTemplate "templates/post-list-page.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  -- Index
  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- fmap (take perPage) . recentFirst =<< loadAllSnapshots postsPattern "body"
      let ctx = listField "posts" (postCtx tags) (return posts)
                <> field "tags" (\_ -> renderTagList tags)
                <> constField "title" "mvr"
                -- <> boolField "isIndex" (const True)
                <> paginateContext pag 1
                <> defaultContext
      getResourceBody
        >>= applyAsTemplate ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "projects.md" $ do
    route (setExtension "html")
    compile $ do
      pandocMathCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  -- Compile templates
  match "templates/*" $ compile templateCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  -- Copy some files verbatim
  match "public/**" $ do
    route $ gsubRoute "public/" (const "")
    compile copyFileCompiler

postCtx :: Tags -> Context String
postCtx tags = dateField "date" "%B %e, %Y"
               <> tagsField "tags" tags
               <> teaserField "teaser" "body"
               <> defaultContext

pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler =
    let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
                          Ext_latex_macros]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr enableExtension defaultExtensions mathExtensions
        writerOptions = defaultHakyllWriterOptions {
                          writerExtensions = newExtensions,
                          writerHTMLMathMethod = MathJax ""
                        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions

config :: Configuration
config = defaultConfiguration
    -- { deployCommand = "rsync --checksum -ave 'ssh -p 2222' \
    --                   \_site/* jaspervdj@jaspervdj.be:jaspervdj.be"
    -- }
