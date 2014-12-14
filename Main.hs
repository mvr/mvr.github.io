{-# LANGUAGE OverloadedStrings  #-}

module Main (main) where

import Data.Monoid
import System.Environment (getArgs)
import qualified Data.Set as S

import Hakyll
import Text.Pandoc.Options

main :: IO ()
main = do
  (action:_) <- getArgs
  run action

-- Much is stolen from https://github.com/jaspervdj/jaspervdj
run :: String -> IO ()
run action = hakyllWith config $ do
  let postsPattern = if action == "watch"
                     then "posts/*" .||. "inprogress/*"
                     else "posts/*"

  tags <- buildTags postsPattern (fromCapture "tags/*.html")

  -- Compile posts
  match postsPattern $ do
    route   $ setExtension ".html"
    compile $ pandocMathCompiler
      >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  -- Post list
  create ["posts.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll postsPattern
      let ctx = constField "title" "Posts" <>
                listField "posts" (postCtx tags) (return posts) <>
                defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/posts.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  -- Index
  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- fmap (take 3) . recentFirst =<< loadAll postsPattern
      let ctx = listField "posts" (postCtx tags) (return posts)
                <> field "Tags" (\_ -> renderTagList tags)
                <> defaultContext
      getResourceBody
        >>= applyAsTemplate ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  -- Tags
  tagsRules tags $ \tag pattern -> do
    let title = "Posts tagged " ++ tag

    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let ctx = constField "title" title <>
                listField "posts" (postCtx tags) (return posts) <>
                defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/posts.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  -- Compile templates
  match "templates/*" $ compile templateCompiler

  -- Copy some files verbatim
  match "public/**" $ do
    route $ gsubRoute "public/" (const "")
    compile copyFileCompiler

postCtx :: Tags -> Context String
postCtx tags = dateField "date" "%B %e, %Y"
               <> tagsField "tags" tags
               <> defaultContext

pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler =
    let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
                          Ext_latex_macros]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr S.insert defaultExtensions mathExtensions
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
