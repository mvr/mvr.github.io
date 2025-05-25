{-# LANGUAGE OverloadedStrings, PartialTypeSignatures #-}

module Bibliography
    ( filterBibliography
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Pandoc.Definition
import Text.Pandoc.Walk (walkM)
import Hakyll
import System.Process
import System.Exit
import System.FilePath (dropExtension, takeFileName, (</>))
import qualified Data.Text.IO as T
import Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Control.Monad (unless)
import System.Directory (createDirectoryIfMissing, doesFileExist)

filterBibliography :: Pandoc -> Compiler Pandoc
filterBibliography = walkM convertBlock

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
          [tempBibFile, "-C", "--csl=" ++ cslPath, "-t", "html"]
          ""

    T.writeFile mdCachePath md
    return md

convertCodeBlock :: Attr -> Text -> Compiler Block
convertCodeBlock (id, extraClasses, namevals) contents = do
  md <- unsafeCompiler (buildBibliography contents)
  let comment = RawBlock (Format "html") ("<!--" `T.append` contents `T.append` "-->")
  
  -- Insert the markdown as raw HTML
  return $ Div (id, "bibliography":extraClasses, namevals) 
           [comment, RawBlock (Format "html") md]

convertBlock :: Block -> Compiler Block
convertBlock (CodeBlock attrs@(id, "bib":extraClasses, namevals) contents) = 
  convertCodeBlock attrs contents
convertBlock x = return x
