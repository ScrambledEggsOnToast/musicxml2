
-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : portable
--
-------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Music.MusicXml.Read.Score (
  ) where

import Prelude hiding (getLine)

import Data.Maybe (maybeToList)
import Data.String
import Data.Semigroup
import Data.Default
import Numeric.Natural

import Text.XML.Light hiding (Line)

import Music.MusicXml.Score
import Music.MusicXml.Time
import Music.MusicXml.Pitch
import Music.MusicXml.Dynamics
import Music.MusicXml.Read
import Music.MusicXml.Write

import qualified Data.List as List
import qualified Data.Char as Char

instance IsString QName where
    fromString = unqual

-- This instance is used by fromXml and should take a single list
instance ReadMusicXml Score where
    readMusicXml [ e@(Element qname attr content _) ] = 
            case qname of
                "score-partwise" -> Partwise scoreAttr header (readPartwise e)
                "timewise-score" -> Timewise scoreAttr header (readTimewise e)
        where
            scoreAttr = readScoreAttrs e
            header = readMusicXml . concat . map (`findChildren` e) 
                   $ ["title", "movement-title", "identification", "part-list"]

readPartwise :: Element -> [(PartAttrs, [(MeasureAttrs, Music)])]
readTimewise :: Element -> [(MeasureAttrs, [(PartAttrs, Music)])]

readPartwise = fmap (\partElem -> (readPartAttrs partElem,
                    fmap (\measureElem -> (readMeasureAttrs measureElem, readMusic measureElem)) $
                        readMeasureElems partElem)) . readPartElems

readTimewise = fmap (\measureElem -> (readMeasureAttrs measureElem,
                    fmap (\partElem -> (readPartAttrs partElem, readMusic partElem)) $
                        readPartElems measureElem)) . readMeasureElems

readPartElems :: Element -> [Element]
readMeasureElems :: Element -> [Element]

readPartElems = findElements "part"
readMeasureElems = findElements "measure"

readMusic = undefined

readPartAttrs = PartAttrs . maybe "" id . findAttr "id"
readMeasureAttrs = MeasureAttrs . read . maybe "0" id . findAttr "number"

readScoreAttrs = ScoreAttrs . maybe [] versionList . findAttr "version"

versionList :: String -> [Int]
versionList "" = []
versionList s = reverse . map read . l [] . reverse $ s
    where
        l n ('.':cs) = n : l [] cs
        l n (c:cs) = l (c:n) cs
        l n [] = [n]

instance ReadMusicXml ScoreHeader where
    readMusicXml = undefined
