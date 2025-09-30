{-# LANGUAGE OverloadedStrings  #-}

module Main (main) where

import Control.Monad (filterM, liftM, (<=<), (>=>), msum)
import Control.Applicative (empty)
import Data.Monoid
import System.Environment (getArgs)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Text.Titlecase

import Data.Time.Clock (UTCTime (..))
import Data.Time.Format (formatTime, parseTimeM)
import Data.Time.Locale.Compat (TimeLocale, defaultTimeLocale)

import Hakyll
import Text.Pandoc.Options
import Text.Pandoc.SideNote
-- import Hakyll.Core.Compiler.Internal (compilerThrow)
-- import Text.Pandoc.Builder (setMeta)
-- import Text.Pandoc as Pandoc (runIO)

import qualified Theorem
import qualified Hyphen
import qualified LifeViewer
import qualified Tikz
import qualified Bibliography

main :: IO ()
main = do
  args <- getArgs
  case args of
    (h:_) -> run h
    []    -> run "blank"

perPage :: Int
perPage = 5

grouper :: (MonadMetadata m, MonadFail m) => (Metadata -> Bool) -> [Identifier] -> m [[Identifier]]
grouper predicate ids = do
  filtered <- filterM (fmap predicate . getMetadata) ids
  fmap (paginateEvery perPage) (sortRecentFirst filtered)

makeId :: PageNumber -> Identifier
makeId pageNum = fromFilePath $ "page/" ++ show pageNum ++ ".html"

compressScssCompiler :: Compiler (Item String)
compressScssCompiler = do
    getResourceString
    >>= withItemBody (unixFilter "sass" [ "--stdin"
                                        , "--load-path", "css"
                                        ])

-- Much is stolen from https://github.com/jaspervdj/jaspervdj
run :: String -> IO ()
run action = hakyllWith config $ do
  let postsPattern = if action == "watch"
                     then "posts/*" .||. "inprogress/*"
                     else "posts/*"
  let postsMetadataFilter m = (action == "watch") || (lookupString "draft" m /= Just "true")

  pag <- buildPaginateWith (grouper postsMetadataFilter) postsPattern makeId
  tags <- buildTags postsPattern (fromCapture "tags/*.html")

  -- Compile posts
  matchMetadata postsPattern postsMetadataFilter $ do
    route   $ setExtension ".html"
    let ctx = postCtx tags -- <> field "tags" (\_ -> renderTagList tags)
    compile $ pandocCustomCompiler
      >>= saveSnapshot "body"
      >>= loadAndApplyTemplate "templates/post.html" ctx
      -- >>= saveSnapshot "rendered"
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
            constField "title" ("Page " ++ show pageNum) <>
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

  let simplePages :: [Identifier]
      simplePages = [ "projects.md", "academics.md" ]

  match (fromList simplePages) $ do
    route (setExtension "html")
    let ctx = defaultContext
    compile $ pandocCustomCompiler
      >>= loadAndApplyTemplate "templates/simple.html" ctx
      >>= loadAndApplyTemplate "templates/post-page.html" ctx
      >>= loadAndApplyTemplate "templates/default.html" ctx
      >>= relativizeUrls

  -- Compile templates
  match "templates/*.html" $ compile templateCompiler

  match "css/*.scss" $ do
        compile getResourceBody

  scssDependency <- makePatternDependency "css/*.scss"
  rulesExtraDependencies [scssDependency] $ create ["css/style.css"] $ do
    route idRoute
    compile $ loadBody "css/style.scss"
      >>= makeItem
      >>= withItemBody (unixFilter "sass" [ "--stdin"
                                        , "--load-path", "css"
                                        ])

  -- Copy some files verbatim
  match "public/**" $ do
    route $ gsubRoute "public/" (const "")
    compile copyFileCompiler

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
        let feedCtx = postCtx tags <> bodyField "description"

        posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "body"
        renderAtom feedConfiguration feedCtx posts

maybeField :: String -> (Item a -> Compiler (Maybe String)) -> Context a
maybeField key value = field key $ maybe empty return <=< value

-- This is all duplicated from Hakyll source, because it can't be customised. So dumb!
getWrittenUTC :: MonadMetadata m
           => Identifier
           -> m (Maybe UTCTime)
getWrittenUTC i = do
  metadata <- getMetadata i

  let tryField k fmt = lookupString k metadata >>= parseTime' fmt
      parseTime' = parseTimeM True defaultTimeLocale
      formats = [ "%a, %d %b %Y %H:%M:%S %Z"
                , "%Y-%m-%dT%H:%M:%S%Z"
                , "%Y-%m-%d %H:%M:%S%Z"
                , "%Y-%m-%d"
                , "%B %e, %Y %l:%M %p"
                , "%B %e, %Y"
                , "%b %d, %Y"
                ]
  return $ msum $ [tryField "written" fmt | fmt <- formats]

writtenField :: String -> String -> Context String
writtenField key format = maybeField key $ \i -> do
    time <- getWrittenUTC $ itemIdentifier i
    return $ fmap (formatTime defaultTimeLocale format) time

postCtx :: Tags -> Context String
postCtx tags = dateField "date" "%B %e, %Y"
               <> writtenField "written" "%B %e, %Y"
               <> tagsField "tags" tags
               <> teaserField "teaser" "body"
               <> defaultContext

pandocCustomCompiler :: Compiler (Item String)
pandocCustomCompiler = do
  -- identifier <- getUnderlying
  -- metadata <- getMetadata identifier
  
  let readerOptions = defaultHakyllReaderOptions {
                        readerExtensions = foldr enableExtension (readerExtensions defaultHakyllReaderOptions) [Ext_mark]
                      }
      writerOptions = defaultHakyllWriterOptions {
                        writerHTMLMathMethod = KaTeX ""
                      }

      -- addMetaFromConfig :: Pandoc -> Compiler Pandoc
      -- addMetaFromConfig doc =
      --   case (lookupString "csl" metadata, lookupString "bibliography" metadata) of
      --     (Just cslFile, Just bibFile) -> do
      --       result <- (unsafeCompiler . Pandoc.runIO . Pandoc.processCitations . setMeta "csl" cslFile . setMeta "bibliography" bibFile) doc
      --       case result of
      --         Left  e -> compilerThrow ["Error during processCitations: " ++ show e]
      --         Right x -> return x
      --     _ -> return doc

      transform =
        -- addMetaFromConfig <=<
        Tikz.filterTikz <=< 
        Bibliography.filterBibliography <=<
        return
        . LifeViewer.filterLifeViewer
        . Hyphen.filterHyphen
        . Theorem.filterThms
        . usingSideNotes

  pandocCompilerWithTransformM readerOptions writerOptions transform

config :: Configuration
config = defaultConfiguration
    { deployCommand = "scripts/deploy.sh"
    }

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "mvr"
    , feedDescription = "Personal Site of Mitchell Riley"
    , feedAuthorName  = "Mitchell Riley"
    , feedAuthorEmail = "mitchell.v.riley@gmail.com"
    , feedRoot        = "http://mvr.github.io"
    }
