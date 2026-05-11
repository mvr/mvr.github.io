{-# LANGUAGE OverloadedStrings #-}

module ServerKaTeX
    ( renderMath
    ) where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, put, runStateT)
import Crypto.Hash.MD5 as MD5
import Data.Bits ((.|.), shiftL)
import Data.Char (digitToInt, isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.Process (readProcess)
import Text.Pandoc.Definition
import Text.Pandoc.Walk (query, walkM)
import Hakyll

data MathItem = MathItem MathType Text

renderMath :: Text -> Pandoc -> Compiler Pandoc
renderMath macroSource doc = do
  let inlineItems = query collectInlineMath doc
      blockItems = query collectRawDisplayMath doc
  doc' <-
    if null inlineItems
      then return doc
      else do
        rendered <- unsafeCompiler (renderKaTeXItems macroSource inlineItems)
        (replacedDoc, leftovers) <- runStateT (walkM replaceMath doc) rendered
        when (not $ null leftovers) $
          fail $ "KaTeX rendering mismatch: " ++ show (length leftovers) ++ " rendered inline items were not consumed."
        return replacedDoc
  if null blockItems
    then return doc'
    else do
      rendered <- unsafeCompiler (renderKaTeXItems macroSource blockItems)
      (replacedDoc, leftovers) <- runStateT (walkM replaceRawDisplayMath doc') rendered
      when (not $ null leftovers) $
        fail $ "KaTeX rendering mismatch: " ++ show (length leftovers) ++ " rendered display blocks were not consumed."
      return replacedDoc

collectInlineMath :: Inline -> [MathItem]
collectInlineMath (Math mathType tex) = [MathItem mathType tex]
collectInlineMath (RawInline fmt tex)
  | isTeXFormat fmt && isDisplayEnvironment tex = [MathItem DisplayMath tex]
collectInlineMath _ = []

collectRawDisplayMath :: Block -> [MathItem]
collectRawDisplayMath (RawBlock fmt tex)
  | isTeXFormat fmt && isDisplayEnvironment tex = [MathItem DisplayMath tex]
collectRawDisplayMath _ = []

replaceMath :: Inline -> StateT [Text] Compiler Inline
replaceMath (Math _ _) = do
  rendered <- get
  case rendered of
    html:rest -> do
      put rest
      return $ RawInline (Format "html") html
    [] ->
      lift $ fail "KaTeX rendering mismatch: exhausted rendered math."
replaceMath (RawInline fmt tex)
  | isTeXFormat fmt && isDisplayEnvironment tex = do
      rendered <- get
      case rendered of
        html:rest -> do
          put rest
          return $ RawInline (Format "html") html
        [] ->
          lift $ fail "KaTeX rendering mismatch: exhausted rendered raw inline math."
replaceMath inline = return inline

replaceRawDisplayMath :: Block -> StateT [Text] Compiler Block
replaceRawDisplayMath (RawBlock fmt tex)
  | isTeXFormat fmt && isDisplayEnvironment tex = do
      rendered <- get
      case rendered of
        html:rest -> do
          put rest
          return $ RawBlock (Format "html") html
        [] ->
          lift $ fail "KaTeX rendering mismatch: exhausted rendered display math."
replaceRawDisplayMath block = return block

isTeXFormat :: Format -> Bool
isTeXFormat (Format fmt) = fmt == "tex" || fmt == "latex"

isDisplayEnvironment :: Text -> Bool
isDisplayEnvironment tex =
  "\\begin{" `T.isPrefixOf` T.stripStart tex

tempDir :: FilePath
tempDir = "_cache/katex"

renderKaTeXItems :: Text -> [MathItem] -> IO [Text]
renderKaTeXItems macroSource items = do
  createDirectoryIfMissing True tempDir

  cacheSalt <- rendererCacheSalt
  let input = encodeRequest macroSource items
      hashInput = cacheSalt <> "\n" <> input
      hash = T.unpack . T.decodeUtf8 . B16.encode . MD5.hash $ T.encodeUtf8 hashInput
      cachePath = tempDir </> (hash ++ ".txt")

  cacheExists <- doesFileExist cachePath
  output <-
    if cacheExists
      then T.readFile cachePath
      else do
        rendered <- T.pack <$> readProcess "node" ["scripts/render-katex.mjs"] (T.unpack input)
        T.writeFile cachePath rendered
        return rendered

  case decodeResponse (length items) output of
    Left err -> fail err
    Right html -> return html

rendererCacheSalt :: IO Text
rendererCacheSalt =
  T.intercalate "\n" <$> traverse readSaltFile
    [ "scripts/render-katex.mjs"
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

encodeRequest :: Text -> [MathItem] -> Text
encodeRequest macroSource items =
  T.unlines $
    [ hexText macroSource
    , T.pack (show (length items))
    ] <> fmap encodeItem items
  where
    encodeItem (MathItem mathType tex) =
      mathTypeCode mathType <> "\t" <> hexText tex

mathTypeCode :: MathType -> Text
mathTypeCode DisplayMath = "D"
mathTypeCode InlineMath = "I"

decodeResponse :: Int -> Text -> Either String [Text]
decodeResponse expected output =
  case T.lines output of
    [] -> Left "KaTeX renderer returned no output."
    countLine:resultLines -> do
      count <- parseCount countLine
      when (count /= expected) $
        Left $ "KaTeX renderer returned " ++ show count ++ " results, expected " ++ show expected ++ "."
      when (length resultLines /= expected) $
        Left $ "KaTeX renderer returned " ++ show (length resultLines) ++ " result lines, expected " ++ show expected ++ "."
      traverse decodeResult resultLines

decodeResult :: Text -> Either String Text
decodeResult line =
  case T.breakOn "\t" line of
    ("ok", encoded) ->
      decodeHexText (T.drop 1 encoded)
    ("err", encoded) -> do
      message <- decodeHexText (T.drop 1 encoded)
      Left $ "KaTeX renderer failed: " ++ T.unpack message
    _ ->
      Left $ "Malformed KaTeX renderer output line: " ++ T.unpack line

parseCount :: Text -> Either String Int
parseCount text =
  case reads (T.unpack text) of
    [(count, "")] -> Right count
    _ -> Left $ "Malformed KaTeX renderer result count: " ++ T.unpack text

hexText :: Text -> Text
hexText = T.decodeUtf8 . B16.encode . T.encodeUtf8

decodeHexText :: Text -> Either String Text
decodeHexText input
  | odd (T.length input) = Left "Odd-length hex string in KaTeX renderer output."
  | otherwise = do
      bytes <- B.pack <$> decodePairs (T.unpack input)
      case T.decodeUtf8' bytes of
        Left err -> Left $ "Invalid UTF-8 in KaTeX renderer output: " ++ show err
        Right txt -> Right txt
  where
    decodePairs [] = Right []
    decodePairs (a:b:rest)
      | isHexDigit a && isHexDigit b =
          let byte = toEnum $ (digitToInt a `shiftL` 4) .|. digitToInt b
          in (byte :) <$> decodePairs rest
      | otherwise =
          Left "Invalid hex digit in KaTeX renderer output."
    decodePairs _ = Left "Odd-length hex string in KaTeX renderer output."
