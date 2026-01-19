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

filterBibliography :: Pandoc -> Compiler Pandoc
filterBibliography doc = do
  let bibContents = query collectBibliography doc
  if null bibContents
    then return doc
    else do
      let counts = fmap countEntries bibContents
          combined = T.intercalate "\n\n" bibContents
      html <- unsafeCompiler (buildBibliography combined)
      chunks <- case splitBibliographyHtml html counts of
        Left err -> fail err
        Right cs -> return cs
      evalStateT (walkM replaceBibBlock doc) chunks

tempDir = "_cache/bib"
tempBibFile = tempDir ++ "/file.bib"
cslPath = "bibstyle.csl"

bibHash :: Text -> String
bibHash input =
  T.unpack $ T.decodeUtf8 $ B16.encode $ MD5.hash $ T.encodeUtf8 input

buildBibliography :: Text -> IO Text
buildBibliography input = do
  createDirectoryIfMissing True tempDir
  
  let hash = bibHash input
  let mdCachePath = tempDir </> (hash ++ ".md")
  
  cacheExists <- doesFileExist mdCachePath
  
  if cacheExists then
    T.readFile mdCachePath
  else do
    T.writeFile tempBibFile input
    md <- T.pack <$> readProcess "pandoc"
          [tempBibFile, "-C", "--katex", "--csl=" ++ cslPath, "-t", "html"]
          ""

    T.writeFile mdCachePath md
    return md

collectBibliography :: Block -> [Text]
collectBibliography (CodeBlock (_, classes, _) contents)
  | "bib" `elem` classes = [contents]
collectBibliography _ = []

replaceBibBlock :: Block -> StateT [Text] Compiler Block
replaceBibBlock (CodeBlock (ident, classes, namevals) contents)
  | "bib" `elem` classes = do
      chunks <- get
      case chunks of
        (current:rest) -> do
          put rest
          let extraClasses = filter (/= "bib") classes
              comment = RawBlock (Format "html") ("<!--" <> contents <> "-->")
          return $
            Div
              (ident, "bibliography" : extraClasses, namevals)
              [comment, RawBlock (Format "html") current]
        [] ->
          lift $ fail "Bibliography conversion mismatch: exhausted rendered chunks."
replaceBibBlock block = return block

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
                 in T.pack . renderTree $ [TagBranch name attrs chunkTrees]
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

isEntryTree :: TagTree String -> Bool
isEntryTree (TagBranch _ attrs _) = hasClass "csl-entry" attrs
isEntryTree _ = False
