{-# LANGUAGE OverloadedStrings, PartialTypeSignatures #-}

module Bibliography
    ( filterBibliography
    ) where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Pandoc.Definition
import Text.Pandoc.Walk (query, walkM)
import Hakyll
import System.Process
import System.Exit
import System.FilePath (dropExtension, takeFileName, (</>))
import qualified Data.Text.IO as T
import Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Control.Applicative ((<|>))
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Text.HTML.TagSoup.Tree (TagTree(..), parseTree, renderTree)

filterBibliography :: Bool -> Pandoc -> Compiler Pandoc
filterBibliography bakeKaTeX doc = do
  let bibBlocks = query collectBibliography doc
  if null bibBlocks
    then return doc
    else do
      let globalBlocks = filter ((== GlobalBib) . bibBlockMode) bibBlocks
          localBlocks = filter ((== LocalBib) . bibBlockMode) bibBlocks
      globalChunks <- renderBibliographyChunks bakeKaTeX globalBlocks
      localChunks <- traverse (renderLocalBibliography bakeKaTeX) localBlocks
      evalStateT (walkM replaceBibBlock doc) (RenderChunks globalChunks localChunks)

tempDir = "_cache/bib"
cslPath = "bibstyle.csl"

bibHash :: Text -> String
bibHash input =
  T.unpack $ T.decodeUtf8 $ B16.encode $ MD5.hash $ T.encodeUtf8 input

buildBibliography :: Bool -> Text -> IO Text
buildBibliography bakeKaTeX input = do
  createDirectoryIfMissing True tempDir
  
  cacheSalt <- bibliographyCacheSalt
  let renderMode = if bakeKaTeX then "katex-html-v4" else "pandoc-katex-span-v1"
  let hash = bibHash (renderMode <> "\n" <> cacheSalt <> "\n" <> input)
  let mdCachePath = tempDir </> (hash ++ ".md")
  let tempBibFile = tempDir </> (hash ++ ".bib")
  let tempHtmlFile = tempDir </> (hash ++ ".html")
  let tempRenderedFile = tempDir </> (hash ++ ".rendered.html")
  
  cacheExists <- doesFileExist mdCachePath
  
  if cacheExists then
    T.readFile mdCachePath
  else do
    T.writeFile tempBibFile input
    rawHtml <- readProcess "pandoc"
          [tempBibFile, "-C", "--katex", "--csl=" ++ cslPath, "-t", "html"]
          ""
    md <-
      if bakeKaTeX
        then do
          T.writeFile tempHtmlFile (T.pack rawHtml)
          callProcess "node" ["scripts/render-katex.mjs", "--html-file", tempHtmlFile, "--out", tempRenderedFile]
          T.readFile tempRenderedFile
        else return (T.pack rawHtml)

    T.writeFile mdCachePath md
    return md

bibliographyCacheSalt :: IO Text
bibliographyCacheSalt =
  T.intercalate "\n" <$> traverse readSaltFile
    [ cslPath
    , "scripts/render-katex.mjs"
    , "package-lock.json"
    , "package.json"
    ]

readSaltFile :: FilePath -> IO Text
readSaltFile path = do
  exists <- doesFileExist path
  if exists
    then do
      contents <- T.readFile path
      return $ T.pack path <> "\n" <> contents
    else return $ T.pack path <> "\n"

data BibMode = GlobalBib | LocalBib
  deriving (Eq)

data BibBlock = BibBlock
  { bibBlockMode :: BibMode
  , bibBlockContents :: Text
  , bibBlockCount :: Int
  }

data RenderChunks = RenderChunks
  { renderGlobalChunks :: [Text]
  , renderLocalChunks :: [Text]
  }

renderBibliographyChunks :: Bool -> [BibBlock] -> Compiler [Text]
renderBibliographyChunks _ [] = return []
renderBibliographyChunks bakeKaTeX bibBlocks = do
  let counts = fmap bibBlockCount bibBlocks
      combined = T.intercalate "\n\n" (fmap bibBlockContents bibBlocks)
  html <- unsafeCompiler (buildBibliography bakeKaTeX combined)
  case splitBibliographyHtml html counts of
    Left err -> fail err
    Right chunks -> return chunks

renderLocalBibliography :: Bool -> BibBlock -> Compiler Text
renderLocalBibliography bakeKaTeX bibBlock = do
  chunks <- renderBibliographyChunks bakeKaTeX [bibBlock]
  case chunks of
    [chunk] -> return chunk
    _ -> fail "Local bibliography conversion mismatch: expected one rendered chunk."

collectBibliography :: Block -> [BibBlock]
collectBibliography (CodeBlock (_, classes, _) contents)
  | Just mode <- bibliographyMode classes = [BibBlock mode contents (countEntries contents)]
collectBibliography _ = []

replaceBibBlock :: Block -> StateT RenderChunks Compiler Block
replaceBibBlock (CodeBlock (ident, classes, namevals) contents)
  | Just mode <- bibliographyMode classes = do
      current <- popRenderedChunk mode
      let extraClasses = filter (`notElem` ["bib", "biblocal"]) classes
          comment = RawBlock (Format "html") ("<!--" <> contents <> "-->")
      return $
        Div
          (ident, "bibliography" : extraClasses, namevals)
          [comment, RawBlock (Format "html") current]
replaceBibBlock block = return block

bibliographyMode :: [Text] -> Maybe BibMode
bibliographyMode classes
  | "biblocal" `elem` classes = Just LocalBib
  | "bib" `elem` classes = Just GlobalBib
  | otherwise = Nothing

popRenderedChunk :: BibMode -> StateT RenderChunks Compiler Text
popRenderedChunk mode = do
  chunks <- get
  case mode of
    GlobalBib ->
      case renderGlobalChunks chunks of
        (current:rest) -> do
          put chunks { renderGlobalChunks = rest }
          return current
        [] -> lift $ fail "Global bibliography conversion mismatch: exhausted rendered chunks."
    LocalBib ->
      case renderLocalChunks chunks of
        (current:rest) -> do
          put chunks { renderLocalChunks = rest }
          return current
        [] -> lift $ fail "Local bibliography conversion mismatch: exhausted rendered chunks."

countEntries :: Text -> Int
countEntries = length . filter isEntryLine . T.lines
  where
    isEntryLine line =
      case T.uncons (T.dropWhile isSpace line) of
        Just ('@', _) -> True
        _ -> False

-- Parse the combined bibliography HTML, then slice the entry tree so each
-- original code block receives the correct subset while retaining global numbering.
splitBibliographyHtml :: Text -> [Int] -> Either String [Text]
splitBibliographyHtml _ [] = Right []
splitBibliographyHtml html counts
  | totalEntries == 0 = Right (replicate (length counts) html)
  | otherwise = do
      branch <- maybe (Left "Could not locate bibliography container in rendered HTML.") Right (findBibliographyBranch trees)
      case branch of
        TagBranch name attrs children -> do
          let leading = takeWhile (not . isEntryTree) children
              rest = dropWhile (not . isEntryTree) children
              (revTrailing, revMiddle) = span (not . isEntryTree) (reverse rest)
              trailing = reverse revTrailing
              middle = reverse revMiddle
          segments <- buildSegments middle
          groupedSegments <- splitSegments counts segments
          let renderChunk segs =
                let chunkTrees = leading <> concatMap flattenSegment segs <> trailing
                    chunkAttrs = removeAttr "id" attrs
                 in T.pack . renderTree $ [TagBranch name chunkAttrs chunkTrees]
          pure $ fmap renderChunk groupedSegments
        _ -> Left "Unexpected HTML node while splitting bibliography output."
  where
    totalEntries = sum counts
    trees = parseTree (T.unpack html)

data EntrySegment = EntrySegment
  { segmentPrefix :: [TagTree String]
  , segmentEntry :: TagTree String
  }

flattenSegment :: EntrySegment -> [TagTree String]
flattenSegment (EntrySegment prefix entry) = prefix ++ [entry]

buildSegments :: [TagTree String] -> Either String [EntrySegment]
buildSegments = fmap reverse . go [] []
  where
    go acc pending [] =
      if null pending
        then Right acc
        else Left "Dangling nodes found after last bibliography entry."
    go acc pending (node:nodes)
      | isEntryTree node =
          go (EntrySegment pending node : acc) [] nodes
      | otherwise =
          go acc (pending ++ [node]) nodes

splitSegments :: [Int] -> [EntrySegment] -> Either String [[EntrySegment]]
splitSegments [] [] = Right []
splitSegments [] _ = Left "Extra bibliography entries remain after splitting."
splitSegments (c:cs) segments
  | c < 0 = Left "Encountered negative bibliography count."
  | otherwise =
      let (current, rest) = splitAt c segments
       in if length current == c
            then (current :) <$> splitSegments cs rest
            else Left "Not enough bibliography entries to satisfy requested chunk sizes."

findBibliographyBranch :: [TagTree String] -> Maybe (TagTree String)
findBibliographyBranch [] = Nothing
findBibliographyBranch (node:nodes) =
  case node of
    branch@(TagBranch _ attrs children)
      | hasClass "csl-bib-body" attrs -> Just branch
      | otherwise ->
          findBibliographyBranch children <|> findBibliographyBranch nodes
    _ -> findBibliographyBranch nodes

hasClass :: String -> [(String, String)] -> Bool
hasClass target attrs =
  case lookup "class" attrs of
    Nothing -> False
    Just cls -> target `elem` words cls

removeAttr :: String -> [(String, String)] -> [(String, String)]
removeAttr name = filter ((/= name) . fst)

isEntryTree :: TagTree String -> Bool
isEntryTree (TagBranch _ attrs _) = hasClass "csl-entry" attrs
isEntryTree _ = False
