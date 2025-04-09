{-# OPTIONS_GHC -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns
    -Wredundant-constraints #-}
{-# LANGUAGE OverloadedStrings, PartialTypeSignatures #-}

module LifeViewer
    ( filterLifeViewer
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Builder hiding (Example)
import Text.Pandoc.Generic

filterLifeViewer :: Pandoc -> Pandoc
filterLifeViewer = topDown convertBlock

convertBlock :: Block -> Block
convertBlock (CodeBlock (ids, classes, kvs) string) | "lifeviewer" `elem` classes = RawBlock "html" 
  (          "<div class=\"lifeviewer\">"
  `T.append` "<textarea>\n"
  `T.append` string
  `T.append` "\n"
  `T.append` "#C Colours are set in src/LifeViewer.hs\n"
  -- `T.append` "#C [[ THUMBSIZE 1 THUMBLAUNCH ]]\n"
  `T.append` "#C [[ NOGUI ]]\n"
  `T.append` "#C [[ COLOR BACKGROUND #f8f8f8 ]]\n"
  `T.append` "#C [[ COLOR ALIVE #000000 ]]\n"
  `T.append` "#C [[ COLOR ALIVERAMP #000000 ]]\n"
  `T.append` "#C [[ COLOR DEADRAMP #dfebf6 ]]\n"
  `T.append` "#C [[ COLOR GRID #f0f0f0 ]]\n"
  `T.append` "#C [[ GRID ]]\n"
  `T.append` "#C [[ GRIDMAJOR 0 ]]\n"
  `T.append` (if "LifeHistory" `T.isInfixOf` string then
                 "#C [[ COLOR HISTORY #dfebf6 ]]\n"
      `T.append` "#C [[ COLOR MARK1 #803300 ]]\n"
      `T.append` "#C [[ COLOR MARKOFF #e0ae8f ]]\n"
      else
                 "#C [[ COLOR DEAD #dfebf6 ]]\n" )
  `T.append` "</textarea>\n"
  `T.append` "<canvas width=\"600px\"></canvas>\n"
  `T.append` "</div>\n")
convertBlock x = x
