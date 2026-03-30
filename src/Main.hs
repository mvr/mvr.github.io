{-# LANGUAGE OverloadedStrings  #-}

module Main (main) where

import Control.Monad (filterM, liftM, (<=<), (>=>), msum)
import Control.Applicative (empty)
import Data.Monoid
import System.Environment (getArgs, getExecutablePath)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (exitFailure)
import System.Process (callProcess)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Text.Titlecase
import qualified Data.Text as T

import Data.Time.Clock (UTCTime (..))
import Data.Time.Format (formatTime, parseTimeM)
import Data.Time.Locale.Compat (TimeLocale, defaultTimeLocale)

import Data.Char (ord)
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUID.V5

import Hakyll
import Text.Pandoc.Options
import Sidenote (usingSideNotes)
-- import Hakyll.Core.Compiler.Internal (compilerThrow)
-- import Text.Pandoc.Builder (setMeta)
-- import Text.Pandoc as Pandoc (runIO)

import qualified Theorem
import qualified Hyphen
import qualified LifeViewer
import qualified Tikz
import qualified Bibliography
import qualified Feed

main :: IO ()
main = do
  checkRepoRoot
  args <- getArgs
  case args of
    ("deploy":rest) -> deploy rest
    (h:_)           -> run h
    []              -> run "blank"

perPage :: Int
perPage = 5

-- PEBKAC
checkRepoRoot :: IO ()
checkRepoRoot = do
  hasPosts <- doesDirectoryExist "posts"
  hasIndex <- doesFileExist "package.yaml"
  if hasPosts && hasIndex
    then pure ()
    else do
      putStrLn "Expected repo root (missing posts/ or package.yaml). Are you in the root directory?"
      exitFailure

deploy :: [String] -> IO ()
deploy extraArgs = do
  exe <- getExecutablePath
  callProcess exe ["clean"]
  callProcess exe ["build"]
  callProcess "scripts/deploy.sh" extraArgs

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
    let ctx = postCtx tags <> siteCtx -- <> field "tags" (\_ -> renderTagList tags)
    compile $ do
      feed <- pandocFeedCompiler
      _ <- saveSnapshot "feed" feed
      pandocCustomCompiler
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
                siteCtx <>
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
            siteCtx <>
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
                siteCtx <>
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
                <> constField "title" "Mitchell Is Typing"
                -- <> boolField "isIndex" (const True)
                <> paginateContext pag 1
                <> siteCtx
                <> defaultContext
      getResourceBody
        >>= applyAsTemplate ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  let simplePages :: [Identifier]
      simplePages = [ "projects.md", "academics.md" ]

  match (fromList simplePages) $ do
    route (setExtension "html")
    let ctx = siteCtx <> defaultContext
    compile $ pandocCustomCompiler
      >>= loadAndApplyTemplate "templates/simple.html" ctx
      >>= loadAndApplyTemplate "templates/post-page.html" ctx
      >>= loadAndApplyTemplate "templates/default.html" ctx
      >>= relativizeUrls

  -- Compile templates
  match ("templates/*.html" .||. "templates/*.xml") $ compile templateCompiler

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
        let iconUrl = feedRoot feedConfiguration ++ "/favicon.png"
            feedCtx = uuidField "uuid"
                      <> rfc3339PublishedField "published"
                      <> postCtx tags
                      <> bodyField "description"
                      <> constField "icon" iconUrl

        posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "feed"
        Feed.customRenderAtom feedConfiguration feedCtx posts

maybeField :: String -> (Item a -> Compiler (Maybe String)) -> Context a
maybeField key value = field key $ maybe empty return <=< value

-- This is all duplicated from Hakyll source, because it can't be customised. So dumb!
dateFormats :: [String]
dateFormats =
  [ "%a, %d %b %Y %H:%M:%S %Z"
  , "%Y-%m-%dT%H:%M:%S%Z"
  , "%Y-%m-%d %H:%M:%S%Z"
  , "%Y-%m-%d"
  , "%B %e, %Y %l:%M %p"
  , "%B %e, %Y"
  , "%b %d, %Y"
  ]

parseDateField :: Metadata -> String -> Maybe UTCTime
parseDateField metadata key = msum [lookupString key metadata >>= parse fmt | fmt <- dateFormats]
  where
    parse fmt = parseTimeM True defaultTimeLocale fmt

getPublishedUTC :: MonadMetadata m
                => Identifier
                -> m (Maybe UTCTime)
getPublishedUTC i = do
  metadata <- getMetadata i
  let keys = ["published", "date", "written"]
  return $ msum (map (parseDateField metadata) keys)

getWrittenUTC :: MonadMetadata m
           => Identifier
           -> m (Maybe UTCTime)
getWrittenUTC i = do
  metadata <- getMetadata i
  return $ parseDateField metadata "written"

writtenField :: String -> String -> Context String
writtenField key format = maybeField key $ \i -> do
    time <- getWrittenUTC $ itemIdentifier i
    return $ fmap (formatTime defaultTimeLocale format) time

rfc3339PublishedField :: String -> Context a
rfc3339PublishedField key = maybeField key $ \i -> do
    time <- getPublishedUTC $ itemIdentifier i
    return $ fmap (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ") time

uuidField :: String -> Context a
uuidField key = field key $ \item -> do
    let identifierPath = toFilePath (itemIdentifier item)
        identifierBytes = fmap (fromIntegral . ord) identifierPath
        uuid = UUID.V5.generateNamed UUID.V5.namespaceURL identifierBytes
    pure (UUID.toString uuid)

postCtx :: Tags -> Context String
postCtx tags = dateField "date" "%B %e, %Y"
               <> writtenField "written" "%B %e, %Y"
               <> tagsField "tags" tags
               <> teaserField "teaser" "body"
               <> siteCtx
               <> defaultContext

siteCtx :: Context a
siteCtx = constField "siteRoot" (feedRoot feedConfiguration)
          <> constField "siteName" (feedTitle feedConfiguration)
          <> canonicalUrlField "canonicalUrl"

canonicalUrlField :: String -> Context a
canonicalUrlField key = field key $ \item -> do
  mRoute <- getRoute $ itemIdentifier item
  let root = feedRoot feedConfiguration
  case mRoute of
    Nothing    -> pure root
    Just route -> pure (root ++ toUrl route)

pandocReaderOptions :: ReaderOptions
pandocReaderOptions =
  defaultHakyllReaderOptions {
    readerExtensions = foldr enableExtension (readerExtensions defaultHakyllReaderOptions) [Ext_mark]
  }

pandocWriterOptions :: WriterOptions
pandocWriterOptions =
  defaultHakyllWriterOptions {
    writerHTMLMathMethod = KaTeX ""
  }

pandocCustomCompiler :: Compiler (Item String)
pandocCustomCompiler = do
  let transform =
        Tikz.filterTikz <=<
        Bibliography.filterBibliography <=<
        return
        . LifeViewer.filterLifeViewer
        . Hyphen.filterHyphen
        . Theorem.filterThms
        . usingSideNotes

  pandocCompilerWithTransformM pandocReaderOptions pandocWriterOptions transform

pandocFeedCompiler :: Compiler (Item String)
pandocFeedCompiler = do
  let transform =
        return . Feed.simplifyFeedPandoc <=<
        Bibliography.filterBibliography <=<
        return
        . Hyphen.filterHyphen
        . Theorem.filterThms

  item <- getResourceBody
  let teaserItem = fmap trimAtMore item
  doc <- readPandocWith pandocReaderOptions teaserItem
  doc' <- traverse transform doc
  return $ writePandocWith pandocWriterOptions doc'

trimAtMore :: String -> String
trimAtMore raw = T.unpack $ fst $ T.breakOn "<!--more-->" (T.pack raw)

config :: Configuration
config = defaultConfiguration

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Mitchell Is Typing"
    , feedDescription = "mvr's Blog"
    , feedAuthorName  = "Mitchell Riley"
    , feedAuthorEmail = "mitchell.v.riley@gmail.com"
    , feedRoot        = "https://mvr.github.io"
    }
