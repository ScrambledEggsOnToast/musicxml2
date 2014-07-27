
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

{-# LANGUAGE TupleSections #-}

module Music.MusicXml.ReadWrite.Score (
  ) where

import Prelude hiding (getLine)

import Text.XML.Expat.Pickle

import Music.MusicXml.Score
import Music.MusicXml.Time
import Music.MusicXml.Pitch
import Music.MusicXml.Dynamics
import Music.MusicXml.Read
import Music.MusicXml.Write

import Data.List (intersperse)
import Data.Char (toUpper, toLower)
import Data.Tuple (swap)
import Control.Applicative

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
        xpDuration

xpMusicForward = 
    xpWrap (MusicForward, \(MusicForward a) -> a) $
    xpElemNodes "forward" $
        xpDuration

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

xpAttributesCommonTime = 
    xpWrap (const (Time CommonTime), const ((), (show 4, show 4))) $ 
    xpElem "time"
        (xpAttrFixed "symbol" "common")
        (xpPair
            (xpElemNodes "beats" $ xpWithDefault (show 4) $ xpContent xpText)
            (xpElemNodes "beat-type" $ xpWithDefault (show 4) $ xpContent xpText))

xpAttributesCutTime = 
    xpWrap (const (Time CutTime), const ((), (show 2, show 2))) $ 
    xpElem "time"
        (xpAttrFixed "symbol" "cut")
        (xpPair
            (xpElemNodes "beats" $ xpWithDefault (show 2) $ xpContent xpText)
            (xpElemNodes "beat-type" $ xpWithDefault (show 2) $ xpContent xpText))

xpAttributesDivTime = 
    xpWrap ( \(beats, beatType) -> Time (DivTime (Beat . read $ beats) (BeatType . read $ beatType))
           , \(Time (DivTime beats beatType)) -> (show . getBeat $ beats, show . getBeatType $ beatType)) $
    xpElemNodes "time" $
    xpPair
        (xpElemNodes "beats" $ xpContent xpText)
        (xpElemNodes "beat-type" $ xpContent xpText)

xpAttributesClef = 
    xpWrap ( \(sign , line) -> Clef (readClefSign sign) (Line . read $ line)
           , \(Clef sign line) -> (show . getLine $ line, showClefSign sign) ) $ 
    xpElemNodes "clef" $
    xpPair
        (xpElemNodes "sign" $ xpContent xpText0)
        (xpElemNodes "line" $ xpContent xpText0)

-- ----------------------------------------------------------------------------------
-- Notes
-- ----------------------------------------------------------------------------------

xpNoteProps = 
    xpWrap ( \( (instrument, voice, typ, dots, accidental, timeMod), 
                (stem, noteHead, noteHeadText, staff, beam , notations),
                lyrics ) -> 
                NoteProps instrument voice typ dots accidental timeMod
                    stem noteHead noteHeadText staff beam notations
                    lyrics,
             \( NoteProps instrument voice typ dots accidental timeMod
                    stem noteHead noteHeadText staff beam notations
                    lyrics) -> 
                ( (instrument, voice, typ, dots, accidental, timeMod), 
                (stem, noteHead, noteHeadText, staff, beam , notations),
                lyrics )) $ 
    xpTriple
    ( xp6Tuple
        xpNoteInstrument
        xpNoteVoice
        xpNoteType
        xpNoteDots
        xpNoteAccidental
        xpNoteTimeMod )
    ( xp6Tuple
        xpNoteStem
        xpNoteHead
        xpNoteHeadText
        xpNoteStaff
        xpNoteBeam
        xpNoteNotations )

        xpNoteLyrics

xpNoteInstrument = xpOption $ xpElemAttrs "instrument" $ xpAttr "id" xpText

xpNoteVoice = xpOption $ xpElemNodes "voice" $ xpContent xpPrim

xpNoteType = xpOption $ 
    xpWrap (swap, swap) $
    xpElem "type" 
        (xpOption $ xpWrap (readNoteSize, showNoteSize) $ xpAttr "size" xpText)
        (xpWrap (readNoteVal, showNoteVal) $ xpContent xpText)

xpNoteDots = 
    xpWrap (fromIntegral . length, (`replicate` ()) . fromIntegral) $
        xpList $
            xpElemNodes "dot" xpUnit

xpNoteAccidental = xpOption $
    xpWrap ( \((cautionary, editorial), accidental) -> (readAccidental accidental, 
    cautionary, editorial)
           , \(accidental, cautionary, editorial) -> ((cautionary, editorial), showAccidental accidental) ) $
    xpElem "accidental"
        (xpPair
            (xpDefault False $ xpAttr "cautionary" $ xpYesNo)
            (xpDefault False $ xpAttr "editorial" $ xpYesNo) )
        (xpContent xpText0)

xpNoteTimeMod = xpOption $
    xpElemNodes "time-modification"
        (xpPair
            (xpElemNodes "actual-notes" $ xpContent xpPrim)
            (xpElemNodes "normal-notes" $ xpContent xpPrim) )

xpNoteStem = xpOption $ 
    xpWrap (readStemDirection, showStemDirection) $ 
    xpElemNodes "stem" $ xpContent xpText0

xpNoteHead = xpOption $
    xpWrap ( \((filled, parentheses), notehead) -> (readNoteHead notehead, 
    filled, parentheses)
           , \(notehead, filled, parentheses) -> ((filled, parentheses), showNoteHead notehead) ) $
    xpElem "notehead"
        (xpPair
            (xpDefault False $ xpAttr "filled" $ xpYesNo)
            (xpDefault False $ xpAttr "parentheses" $ xpYesNo) )
        (xpContent xpText0)

xpNoteHeadText = xpLift Nothing -- TODO

xpNoteStaff = xpLift Nothing -- TODO

xpNoteBeam = xpOption $ 
    xpElem "beam" 
        (xpWrap (Level . fromIntegral . read, show . getLevel) $ xpDefault "1" $ xpAttr "number" xpText0)
        (xpWrap (readBeamType, showBeamType) $ xpElemNodes "beam" $ xpContent xpText0)

xpNoteNotations = xpDefault [] $ xpElemNodes "notations" $ xpList xpNotation

xpNoteLyrics = xpLift [] -- TODO

xpFullNote = 
    xpAlt (\s -> case s of
        Pitched False _ -> 0
        Unpitched False _ -> 1
        Rest False _ -> 2
        Pitched True _ -> 3
        Unpitched True _ -> 4
        Rest True _ -> 5)
    ([id, chorded] <*> [xpFullNotePitched, xpFullNoteUnpitched, xpFullNoteRest])
  where
    chorded = xpWrap (truify . snd, ((),) . falsify) . xpPair (xpElemNodes "chord" xpUnit)
    truify (Pitched _ a) = Pitched True a
    truify (Unpitched _ a) = Unpitched True a
    truify (Rest _ a) = Rest True a
    falsify (Pitched _ a) = Pitched False a
    falsify (Unpitched _ a) = Unpitched False a
    falsify (Rest _ a) = Rest False a

xpFullNotePitched :: PU [UNode String] FullNote
xpFullNotePitched = 
    xpWrap ( Pitched False, \(Pitched _ pitch) -> pitch ) $
    xpElemNodes "pitch" $
        xpTriple
            (xpElemNodes "step" $ xpContent xpPrim)
            (xpOption $ xpWrap (Semitones, getSemitones) $ xpElemNodes "alter" $ xpContent xpPrim)
            (xpWrap (Octaves, getOctaves) $ xpElemNodes "octave" $ xpContent xpPrim)

xpFullNoteUnpitched = 
    xpWrap ( Unpitched False, \(Unpitched _ displayPitch) -> displayPitch ) $
    xpElemNodes "unpitched" $ 
        xpOption $ xpPair
            (xpElemNodes "display-step" $ xpContent xpPrim)
            (xpWrap (Octaves, getOctaves) $ xpElemNodes "display-octave" $ xpContent xpPrim)

xpFullNoteRest = 
    xpWrap ( Rest False, \(Rest _ displayPitch) -> displayPitch ) $
    xpElemNodes "rest" $ 
        xpOption $ xpPair
            (xpElemNodes "display-step" $ xpContent xpPrim)
            (xpWrap (Octaves, getOctaves) $ xpElemNodes "display-octave" $ xpContent xpPrim)

xpNote = 
    xpAlt (\s -> case s of
        Note _ _ _ _ -> 0
        CueNote _ _ _ -> 1
        GraceNote _ _ _ -> 2)
    [xpNoteNormal, xpNoteCue, xpNoteGrace]

xpNoteNormal = 
    xpWrap ( \(full, dur, ties, props) -> Note full dur ties props
           , \(Note full dur ties props) -> (full, dur, ties, props) ) $
    xp4Tuple
        xpFullNote
        xpDuration
        (xpList xpTie)
        xpNoteProps

xpNoteCue = 
    xpWrap ( \(full, dur, props) -> CueNote full dur props
           , \(CueNote full dur props) -> (full, dur, props) ) $
    xpTriple
        xpFullNote
        xpDuration
        xpNoteProps

xpNoteGrace = 
    xpWrap ( \(full, ties, props) -> GraceNote full ties props
           , \(GraceNote full ties props) -> (full, ties, props) ) $
    xpTriple 
        xpFullNote
        (xpList xpTie)
        xpNoteProps

xpDuration = xpWrap (Divs, getDivs) $ xpElemNodes "duration" $ xpContent xpPrim

xpTie = xpWrap (readTie, showTie) $ xpElemAttrs "tie" $ xpAttr "type" xpText

-- ----------------------------------------------------------------------------------
-- Notations
-- ----------------------------------------------------------------------------------

xpNotation = undefined

xpDirection = undefined

xpYesNo = xpWrap 
    ( \b -> case b of
        "yes" -> True
        "no" -> False,
      \b -> if b then "yes" else "no") xpPrim

readNoteSize = undefined
showNoteSize = undefined

readNoteVal = undefined
showNoteVal = undefined

readStartStop = undefined
showStartStop = undefined

readGroupSymbol = undefined
showGroupSymbol = undefined

readGroupBarlines = undefined
showGroupBarlines = undefined

readClefSign = undefined
showClefSign = undefined

readAccidental = undefined
showAccidental = undefined

readStemDirection = undefined
showStemDirection = undefined

readNoteHead = undefined
showNoteHead = undefined

readBeamType = undefined
showBeamType = undefined

readTie = undefined
showTie = undefined
