{-# LANGUAGE OverloadedStrings  #-}

module Main (main) where

import Data.Monoid

import Hakyll

main :: IO ()
main = hakyllWith config $ do
  -- Compile posts
  match "posts/*" $ do
    route   $ setExtension ".html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  -- Post list
  create ["posts.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let ctx = constField "title" "Posts" <>
                listField "posts" postCtx (return posts) <>
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

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

config :: Configuration
config = defaultConfiguration
    -- { deployCommand = "rsync --checksum -ave 'ssh -p 2222' \
    --                   \_site/* jaspervdj@jaspervdj.be:jaspervdj.be"
    -- }
