{-# LANGUAGE OverloadedStrings, PartialTypeSignatures #-}

module Tikz
    ( filterTikz
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Definition
import Text.Pandoc.Walk (walkM)
import Hakyll
import qualified Network.URI.Encode as URI (encodeText)
import System.Process
import System.Exit
import System.FilePath (dropExtension)
import qualified Data.Text.IO as T
import Text.Pandoc (topDown)

filterTikz :: Pandoc -> Compiler Pandoc
filterTikz = walkM convertBlock

-- Mashing https://taeer.bar-yam.me/blog/posts/hakyll-tikz/
-- With https://github.com/the1lab/1lab/blob/main/support/shake/app/Shake/Diagram.hs

tempDir = "tikz_temp"

templatePath = "templates/diagram.tex"
filledPath = tempDir ++ "/filled_template.tex"

buildDiagram :: FilePath -> Text -> IO Text
buildDiagram templatePath input = do
  template <- T.readFile templatePath

  let texContent = T.replace "__BODY__" input
                 -- . T.replace "__PREAMBLE__" preamble
                 $ template

  T.writeFile filledPath texContent

  -- Run pdflatex
  -- TODO: report error
  _ <- readProcess "pdflatex" ["-output-directory=" ++ tempDir, "-synctex=1", "-interaction=nonstopmode", filledPath] ""

  let pdfPath = dropExtension filledPath ++ ".pdf"
  let svgPath = dropExtension filledPath ++ ".svg"

  _ <- readProcess "pdftocairo" ["-svg", pdfPath, svgPath] ""
  T.readFile svgPath

convertCodeBlock :: FilePath -> Attr -> Text -> Compiler Block
convertCodeBlock templatePath (id, extraClasses, namevals) contents = do
  svg <- unsafeCompiler (buildDiagram templatePath contents)

  let encoded = "data:image/svg+xml;utf8," <> URI.encodeText (T.filter (/= '\n') svg)
  return $ Div ("", ["figure"], []) [Plain [Image (id, "tikzpicture":extraClasses, namevals) [] (encoded, "")]]

  -- The problem with just doing the following is that ids in
  -- different SVG images on the same page interfere with each other
  -- (??????)

  -- return $ Div ("", ["figure"], []) [RawBlock "html" svg]

convertBlock :: Block -> Compiler Block
convertBlock (CodeBlock attrs@(id, "tikzpicture":extraClasses, namevals) contents) = convertCodeBlock "templates/diagram.tex" attrs contents
convertBlock (CodeBlock attrs@(id, "tikzcd":extraClasses, namevals) contents) = convertCodeBlock "templates/cd.tex" attrs contents
convertBlock x = return x
