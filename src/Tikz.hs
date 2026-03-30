{-# LANGUAGE OverloadedStrings, PartialTypeSignatures #-}

module Tikz
    ( filterTikz
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Pandoc.Definition
import Text.Pandoc.Walk (walkM)
import Hakyll
import qualified Network.URI.Encode as URI (encodeText)
import System.Process
import System.Exit
import System.FilePath (dropExtension, takeFileName, (</>))
import qualified Data.Text.IO as T
import Text.Pandoc (topDown)
import Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Control.Monad (unless)
import System.Directory (createDirectoryIfMissing, doesFileExist)

filterTikz :: Text -> Pandoc -> Compiler Pandoc
filterTikz macros = walkM (convertBlock macros)

-- Mashing https://taeer.bar-yam.me/blog/posts/hakyll-tikz/
-- With https://github.com/the1lab/1lab/blob/main/support/shake/app/Shake/Diagram.hs

tempDir = "_cache/tikz"
filledPath = tempDir ++ "/filled_template.tex"

diagramHash :: Text -> Text -> String
diagramHash templateContent input =
  T.unpack $ T.decodeUtf8 $ B16.encode $ MD5.hash $ T.encodeUtf8 $ input `T.append` templateContent

buildDiagram :: FilePath -> Text -> Text -> IO Text
buildDiagram templatePath macros input = do
  createDirectoryIfMissing True tempDir
  
  template <- T.readFile templatePath
  
  let hash = diagramHash template (macros `T.append` input)
  let svgCachePath = tempDir </> (hash ++ ".svg")
  
  cacheExists <- doesFileExist svgCachePath
  
  if cacheExists
    then
      T.readFile svgCachePath
    else do
      let texContent =
            T.replace "__BODY__" input $
            T.replace "__MACROS__" macros template
      
      T.writeFile filledPath texContent
      
      _ <- readProcess "pdflatex" ["-output-directory=" ++ tempDir, "-synctex=1", "-interaction=nonstopmode", filledPath] ""
      
      let pdfPath = dropExtension filledPath ++ ".pdf"
      let svgPath = dropExtension filledPath ++ ".svg"
      
      _ <- readProcess "pdftocairo" ["-svg", pdfPath, svgPath] ""
      svg <- T.readFile svgPath
      
      T.writeFile svgCachePath svg
      
      return svg

convertCodeBlock :: Text -> FilePath -> Attr -> Text -> Compiler Block
convertCodeBlock macros templatePath (id, extraClasses, namevals) contents = do
  svg <- unsafeCompiler (buildDiagram templatePath macros contents)
  let comment = RawBlock (Format "html") ("<!--" `T.append` contents `T.append` "-->")
  let encoded = "data:image/svg+xml;utf8," <> URI.encodeText (T.filter (/= '\n') svg)
  return $ Div ("", ["figure"], []) [comment, Plain [Image (id, "tikzpicture":extraClasses, namevals) [] (encoded, "")]]

  -- The problem with just doing the following is that ids in
  -- different SVG images on the same page interfere with each other
  -- (??????)

  -- return $ Div ("", ["figure"], []) [RawBlock "html" svg]

convertBlock :: Text -> Block -> Compiler Block
convertBlock macros (CodeBlock attrs@(id, "tikzpicture":extraClasses, namevals) contents) =
  convertCodeBlock macros "templates/diagram.tex" attrs contents
convertBlock macros (CodeBlock attrs@(id, "tikzcd":extraClasses, namevals) contents) =
  convertCodeBlock macros "templates/cd.tex" attrs contents
convertBlock macros (CodeBlock attrs@(id, "mathpar":extraClasses, namevals) contents) =
  convertCodeBlock macros "templates/mathpar.tex" attrs contents
convertBlock _ x = return x
