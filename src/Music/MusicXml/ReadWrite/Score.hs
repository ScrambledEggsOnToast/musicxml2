
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

module Music.MusicXml.ReadWrite.Score (
  ) where

import Text.XML.Expat.Pickle

import Music.MusicXml.Score
import Music.MusicXml.Time
import Music.MusicXml.Pitch
import Music.MusicXml.Dynamics
import Music.MusicXml.Read
import Music.MusicXml.Write

import Data.List (intersperse)
import Data.Char (toUpper, toLower)

xpScore :: PU [UNode String] Score
xpScore = xpAlt 
    (\s -> case s of
        Partwise _ _ _ -> 0
        Timewise _ _ _ -> 1)
    [xpPartwise, xpTimewise]

xpPartwise = 
    xpWrap ( \(attr, (header, parts)) -> Partwise attr header parts
           , \(Partwise attr header parts) -> (attr, (header, parts))) $
    xpElem "score-partwise"
        xpScoreAttrs
        (xpPair
            xpScoreHeader
            (xpList . xpPartWith . xpList . xpMeasureWith $ xpMusic))

xpTimewise = 
    xpWrap ( \(attr, (header, measures)) -> Timewise attr header measures
           , \(Timewise attr header measures) -> (attr, (header, measures))) $
    xpElem "timewise-score"
        xpScoreAttrs
        (xpPair
            xpScoreHeader
            (xpList . xpMeasureWith . xpList . xpPartWith $ xpMusic))

xpPartWith = xpElem "part" xpPartAttrs
xpMeasureWith = xpElem "measure" xpMeasureAttrs

xpScoreAttrs = 
    xpAttr "version" $ 
        xpWrap (ScoreAttrs . readVersion, \(ScoreAttrs ns) -> showVersion ns) 
            xpText0

readVersion :: String -> [Int]
readVersion = reverse . l "" . reverse
    where
        l cs "" = [read cs]
        l cs ('.':vs) = read cs : l "" vs
        l cs (v:vs) = l (v:cs) vs

showVersion :: [Int] -> String
showVersion = concat . intersperse "." . map show

xpPartAttrs = xpAttr "id" $ xpWrap (PartAttrs, \(PartAttrs i) -> i) xpText0

xpMeasureAttrs = xpAttr "number" $ xpWrap (MeasureAttrs . read, \(MeasureAttrs n) -> show n) xpText0

xpScoreHeader = 
    xpWrap ( \(title, mvm, ident, partList) -> ScoreHeader title mvm ident partList
           , \(ScoreHeader title mvm ident partList) -> (title, mvm, ident, partList)) $
    xp4Tuple
        (xpOption $ xpElemNodes "title" $ xpContent xpText)
        (xpOption $ xpElemNodes "movement-title" $ xpContent xpText)
        (xpOption $ xpElemNodes "identification" $ xpIdentification)
        (xpElemNodes "part-list" xpPartList)

xpIdentification = 
    xpWrap ( Identification, \(Identification cs) -> cs) $
    xpList $
        xpWrap ( \(t, n) -> Creator t n, \(Creator t n) -> (t, n) ) $
        xpElem "creator"
            (xpAttr "type" xpText0)
            (xpContent xpText0)

-- ----------------------------------------------------------------------------------
-- Part list
-- ----------------------------------------------------------------------------------

xpPartList = 
    xpWrap ( PartList, getPartList ) $
    xpList $ xpAlt
        (\s -> case s of
            Part _ _ _ -> 0
            Group _ _ _ _ _ _ _ -> 1)
        [xpPart, xpGroup]

xpPart = 
    xpWrap ( \(ident, (name, abbrev)) -> Part ident name abbrev
           , \(Part ident name abbrev) -> (ident, (name, abbrev))) $
    xpElem "score-part" 
        (xpAttr "id" xpText0)
        (xpPair 
            (xpElemNodes "part-name" $ xpContent xpText0)
            (xpOption $ xpElemNodes "part-abbreviation" $ xpContent xpText))

xpGroup = 
    xpWrap ( \((level, startStop), (name, abbrev, symbol, barlines)) -> Group level startStop name abbrev symbol barlines False
           , \(Group level startStop name abbrev symbol barlines _) -> ((level, startStop), (name, abbrev, symbol, barlines))) $
    xpElem "part-group"
        (xpPair
            (xpWrap (Level . fromIntegral . read, show . getLevel) $ xpAttr "number" xpText0)
            (xpWrap (readStartStop, showStartStop) $ xpAttr "type" xpText0)
        )
        (xp4Tuple
            (xpElemNodes "group-name" $ xpContent xpText0)
            (xpOption $ xpElemNodes "group-abbreviation" $ xpContent xpText)
            (xpOption $ xpWrap (readGroupSymbol, showGroupSymbol) $ xpElemNodes "group-symbol" $ xpContent xpText)
            (xpOption $ xpWrap (readGroupBarlines, showGroupBarlines) $ xpElemNodes "group-barline" $ xpContent xpText)
        )

-- ----------------------------------------------------------------------------------
-- Music
-- ----------------------------------------------------------------------------------

xpMusic = xpWrap (Music, getMusic) $ xpList xpMusicElem

xpMusicElem = 
    xpAlt (\s -> case s of
        MusicAttributes _ -> 0
        MusicBackup _ -> 1
        MusicForward _ -> 2
        MusicNote _ -> 3
        MusicDirection _ -> 4)
    [xpMusicAttributes, xpMusicBackup, xpMusicForward, xpMusicNote, xpMusicDirection]

xpMusicAttributes = 
    xpWrap (MusicAttributes, \(MusicAttributes a) -> a) $ 
    xpElemNodes "attributes" xpAttributes

xpMusicBackup = 
    xpWrap (MusicBackup, \(MusicBackup a) -> a) $
    xpElemNodes "backup" $
        xpElemNodes "duration" xpDuration

xpMusicForward = 
    xpWrap (MusicForward, \(MusicForward a) -> a) $
    xpElemNodes "forward" $
        xpElemNodes "duration" xpDuration

xpMusicNote = 
    xpWrap (MusicNote, \(MusicNote a) -> a) $
    xpElemNodes "note" xpNote

xpMusicDirection = 
    xpWrap (MusicDirection, \(MusicDirection a) -> a) $
    xpElemNodes "direction" $
        xpElemNodes "direction-type" xpDirection

-- ----------------------------------------------------------------------------------
-- Attributes
-- ----------------------------------------------------------------------------------

xpAttributes = 
    xpAlt (\s -> case s of
        Divisions _ -> 0
        Key _ _ -> 1
        Time (CommonTime) -> 2
        Time (CutTime) -> 3
        Time (DivTime _ _) -> 4
        Clef _ _ -> 5)
    [xpAttributesDivisions, xpAttributesKey, xpAttributesCommonTime, xpAttributesCutTime, xpAttributesDivTime, xpAttributesClef]

xpAttributesDivisions = 
    xpWrap (Divisions . Divs . read, (\(Divisions d) -> show $ getDivs d)) $
    xpElemNodes "divisions" $ xpContent xpText0

xpAttributesKey = 
    xpWrap (\(fifths, mode) -> Key fifths mode, \(Key fifths mode) -> (fifths, mode)) $
    xpElemNodes "key" $ 
        xpPair
            (xpElemNodes "fifths" xpFifths)
            (xpElemNodes "mode" xpMode)

xpFifths = xpWrap (Fifths . read , show . getFifths) $ xpContent xpText0

xpMode = xpWrap (readMode, showMode) $ xpContent xpText0

readMode :: String -> Mode
readMode s = case s of
    "none" -> NoMode
    (x:xs) -> read (toUpper x : xs)

showMode :: Mode -> String
showMode NoMode = "none"
showMode x = map toLower . show $ x

xpAttributesCommonTime = undefined

xpAttributesCutTime = undefined

xpAttributesDivTime = undefined

xpAttributesClef = undefined

xpNote = undefined

xpDuration = undefined

xpDirection = undefined

readStartStop = undefined
showStartStop = undefined

readGroupSymbol = undefined
showGroupSymbol = undefined

readGroupBarlines = undefined
showGroupBarlines = undefined
