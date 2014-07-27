
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

import Text.XML.Expat.Pickle hiding (Attributes)

import Music.MusicXml.Score
import Music.MusicXml.Time
import Music.MusicXml.Pitch
import Music.MusicXml.Dynamics
import Music.MusicXml.ReadWrite

import Data.List (intersperse, elemIndex, find)
import Data.Char (toUpper, toLower)
import Data.Tuple (swap)
import Control.Applicative

instance ReadMusicXml Score where
    readMusicXml = unpickleTree xpScore
instance ShowMusicXml Score where
    showMusicXml = pickleTree xpScore

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

instance ReadMusicXml ScoreHeader where
    readMusicXml = unpickleTree xpScoreHeader
instance ShowMusicXml ScoreHeader where
    showMusicXml = pickleTree xpScoreHeader

xpScoreHeader = 
    xpWrap ( \(title, mvm, ident, partList) -> ScoreHeader title mvm ident partList
           , \(ScoreHeader title mvm ident partList) -> (title, mvm, ident, partList)) $
    xp4Tuple
        (xpOption $ xpElemNodes "title" $ xpContent xpText)
        (xpOption $ xpElemNodes "movement-title" $ xpContent xpText)
        (xpOption $ xpElemNodes "identification" $ xpIdentification)
        (xpElemNodes "part-list" xpPartList)

instance ReadMusicXml Identification where
    readMusicXml = unpickleTree xpIdentification
instance ShowMusicXml Identification where
    showMusicXml = pickleTree xpIdentification

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

instance ReadMusicXml PartList where
    readMusicXml = unpickleTree xpPartList
instance ShowMusicXml PartList where
    showMusicXml = pickleTree xpPartList

xpPartList = 
    xpWrap ( PartList, getPartList ) $
    xpList xpPartListElem

instance ReadMusicXml PartListElem where
    readMusicXml = unpickleTree xpPartListElem
instance ShowMusicXml PartListElem where
    showMusicXml = pickleTree xpPartListElem

xpPartListElem = 
    xpAlt (\s -> case s of
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
            (xpWrapStartStop $ xpAttr "type" xpText0)
        )
        (xp4Tuple
            (xpElemNodes "group-name" $ xpContent xpText0)
            (xpOption $ xpElemNodes "group-abbreviation" $ xpContent xpText)
            (xpOption $ xpWrapGroupSymbol $ xpElemNodes "group-symbol" $ xpContent xpText)
            (xpOption $ xpWrapGroupBarlines $ xpElemNodes "group-barline" $ xpContent xpText)
        )

-- ----------------------------------------------------------------------------------
-- Music
-- ----------------------------------------------------------------------------------

xpMusic = xpWrap (Music, getMusic) $ xpList xpMusicElem

instance ReadMusicXml MusicElem where
    readMusicXml = unpickleTree xpMusicElem
instance ShowMusicXml MusicElem where
    showMusicXml = pickleTree xpMusicElem

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

instance ReadMusicXml Attributes where
    readMusicXml = unpickleTree xpAttributes
instance ShowMusicXml Attributes where
    showMusicXml = pickleTree xpAttributes

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
    xpWrap ( \(sign , line) -> Clef sign (Line . read $ line)
           , \(Clef sign line) -> (sign, show . getLine $ line) ) $ 
    xpElemNodes "clef" $
    xpPair
        (xpWrapClefSign $ xpElemNodes "sign" $ xpContent xpText0)
        (xpElemNodes "line" $ xpContent xpText0)

-- ----------------------------------------------------------------------------------
-- Notes
-- ----------------------------------------------------------------------------------

instance ReadMusicXml NoteProps where
    readMusicXml = unpickleTree xpNoteProps
instance ShowMusicXml NoteProps where
    showMusicXml = pickleTree xpNoteProps

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
        (xpOption $ xpWrapNoteSize $ xpAttr "size" xpText)
        (xpWrapNoteVal $ xpContent xpText)

xpNoteDots = 
    xpWrap (fromIntegral . length, (`replicate` ()) . fromIntegral) $
        xpList $
            xpElemNodes "dot" xpUnit

xpNoteAccidental = xpOption $
    xpWrap ( \((cautionary, editorial), accidental) -> (accidental, 
    cautionary, editorial)
           , \(accidental, cautionary, editorial) -> ((cautionary, editorial), accidental) ) $
    xpElem "accidental"
        (xpPair
            (xpDefault False $ xpAttr "cautionary" $ xpYesNo)
            (xpDefault False $ xpAttr "editorial" $ xpYesNo) )
        (xpWrapAccidental $ xpContent xpText0)

xpNoteTimeMod = xpOption $
    xpElemNodes "time-modification"
        (xpPair
            (xpElemNodes "actual-notes" $ xpContent xpPrim)
            (xpElemNodes "normal-notes" $ xpContent xpPrim) )

xpNoteStem = xpOption $ 
    xpWrapStemDirection $ 
    xpElemNodes "stem" $ xpContent xpText0

xpNoteHead = xpOption $
    xpWrap ( \((filled, parentheses), notehead) -> (notehead, 
    filled, parentheses)
           , \(notehead, filled, parentheses) -> ((filled, parentheses), notehead) ) $
    xpElem "notehead"
        (xpPair
            (xpDefault False $ xpAttr "filled" $ xpYesNo)
            (xpDefault False $ xpAttr "parentheses" $ xpYesNo) )
        (xpWrapNoteHead $ xpContent xpText0)

xpNoteHeadText = xpThrow "note head text" -- TODO

xpNoteStaff = xpThrow "note staff" -- TODO

xpNoteBeam = xpOption $ 
    xpElem "beam" 
        (xpWrap (Level . fromIntegral . read, show . getLevel) $ xpDefault "1" $ xpAttr "number" xpText0)
        (xpWrapBeamType $ xpElemNodes "beam" $ xpContent xpText0)

xpNoteNotations = xpDefault [] $ xpElemNodes "notations" $ xpList xpNotation

xpNoteLyrics = xpThrow "note lyrics" -- TODO

instance ReadMusicXml FullNote where
    readMusicXml = unpickleTree xpFullNote
instance ShowMusicXml FullNote where
    showMusicXml = pickleTree xpFullNote

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

instance ReadMusicXml Note where
    readMusicXml = unpickleTree xpNote
instance ShowMusicXml Note where
    showMusicXml = pickleTree xpNote

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

xpTie = xpWrapTie $ xpElemAttrs "tie" $ xpAttr "type" xpText

-- ----------------------------------------------------------------------------------
-- Notations
-- ----------------------------------------------------------------------------------

instance ReadMusicXml Notation where
    readMusicXml = unpickleTree xpNotation
instance ShowMusicXml Notation where
    showMusicXml = pickleTree xpNotation

xpNotation = 
    xpAlt (\s -> case s of
        Tied _ -> 0
        Slur _ _ -> 1
        Tuplet _ _ -> 2
        Glissando _ _ _ _ -> 3
        Slide _ _ _ _ -> 4
        Ornaments _ -> 5
        Technical _ -> 6
        Articulations _ -> 7
        DynamicNotation _ -> 8
        Fermata _ -> 9
        Arpeggiate -> 10
        NonArpeggiate -> 11
        AccidentalMark _ -> 12
        OtherNotation _ -> 13)
    [ xpNotationTied, xpNotationSlur, xpNotationTuplet, xpNotationGlissando
    , xpNotationSlide, xpNotationOrnaments, xpNotationTechnical, xpNotationArticulation
    , xpNotationDynamic, xpNotationFermata, xpNotationArpeggiate, xpNotationNonArpeggiate
    , xpNotationAccidentalMark, xpNotationOther]

xpNotationTied = 
    xpWrap (Tied , \(Tied typ) -> typ) $
    xpWrapStartStopContinue $ xpElemAttrs "tied" $ xpAttr "type" xpText

xpNotationSlur =
    xpWrap (\(level, typ) -> Slur level typ, \(Slur level typ) -> (level, typ)) $
    xpElemAttrs "slur" $
        xpPair
            (xpWrap (Level . fromIntegral . read, show . getLevel) $ xpAttr "number" xpText0)
            (xpWrapStartStopContinue $ xpAttr "type" xpText)

xpNotationTuplet = 
    xpWrap (\(level, typ) -> Tuplet level typ, \(Tuplet level typ) -> (level, typ)) $
    xpElemAttrs "tuplet" $
        xpPair
            (xpWrap (Level . fromIntegral . read, show . getLevel) $ xpAttr "number" xpText0)
            (xpWrapStartStopContinue $ xpAttr "type" xpText)

xpNotationGlissando = 
    xpWrap ( \((level, typ, lineTyp), text) -> Glissando level typ lineTyp text
           , \(Glissando level typ lineTyp text) -> ((level, typ, lineTyp), text) ) $
    xpElem "glissando"
        (xpTriple
            (xpWrap (Level . fromIntegral . read, show . getLevel) $ xpAttr "number" xpText0)
            (xpWrapStartStopContinue $ xpAttr "type" xpText)
            (xpWrapLineType $ xpAttr "line-type" xpText))
        (xpOption $ xpContent xpText)

xpNotationSlide = 
    xpWrap ( \((level, typ, lineTyp), text) -> Slide level typ lineTyp text
           , \(Slide level typ lineTyp text) -> ((level, typ, lineTyp), text) ) $
    xpElem "slide"
        (xpTriple
            (xpWrap (Level . fromIntegral . read, show . getLevel) $ xpAttr "number" xpText0)
            (xpWrapStartStopContinue $ xpAttr "type" xpText)
            (xpWrapLineType $ xpAttr "line-type" xpText))
        (xpOption $ xpContent xpText)

xpNotationOrnaments = 
    xpWrap (Ornaments, \(Ornaments o) -> o) $ 
    xpElemNodes "ornaments" $ xpList $ 
        xpPair 
            xpOrnament
            (xpList xpAccidental)

xpNotationTechnical = 
    xpWrap (Technical, \(Technical t) -> t) $
    xpElemNodes "technical" $ xpList xpTechnical

xpNotationArticulation = 
    xpWrap (Articulations, \(Articulations a) -> a) $
    xpElemNodes "articulations" $ xpList xpArticulation

xpNotationDynamic = 
    xpWrap (DynamicNotation, \(DynamicNotation d) -> d) $
    xpElemNodes "dynamics" $ xpDynamics

xpNotationFermata = 
    xpWrap (Fermata, \(Fermata f) -> f) $
    xpElemNodes "fermata" $ xpContent xpFermataSign

xpNotationArpeggiate =
    xpWrap ((`seq` Arpeggiate), (`seq` ())) $
    xpElemNodes "arpeggiate" xpUnit

xpNotationNonArpeggiate =
    xpWrap ((`seq` NonArpeggiate), (`seq` ())) $
    xpElemNodes "non-arpeggiate" xpUnit

xpNotationAccidentalMark = 
    xpWrap (AccidentalMark, \(AccidentalMark a) -> a) xpAccidental

xpNotationOther = xpThrow "other notation" -- TODO

instance ReadMusicXml Ornament where
    readMusicXml = unpickleTree xpOrnament
instance ShowMusicXml Ornament where
    showMusicXml = pickleTree xpOrnament

xpOrnament = 
    xpAlt (\s -> case s of
        Tremolo _ -> 1
        _ -> 0)
    [ xpEmptyNodes 
        [ (TrillMark, "trill-mark")
        , (Turn, "turn")
        , (DelayedTurn, "delayed-turn")
        , (InvertedTurn, "inverted-turn")
        , (DelayedInvertedTurn, "delayed-inverted-turn")
        , (VerticalTurn, "vertical-turn")
        , (Shake, "shake")
        , (WavyLine, "wavyline")
        , (Mordent, "mordent")
        , (InvertedMordent, "inverted-mordent")
        , (Schleifer, "schleifer")
        ]
    , xpWrap (Tremolo . fromIntegral . read, \(Tremolo n) -> show n) $
        xpElemNodes "tremolo" $ xpContent xpText
    ]

instance ReadMusicXml Technical where
    readMusicXml = unpickleTree xpTechnical
instance ShowMusicXml Technical where
    showMusicXml = pickleTree xpTechnical

xpTechnical = 
    xpAlt (\s -> case s of
        OtherTechnical _ -> 1
        _ -> 0)
    [ xpEmptyNodes 
        [ (UpBow, "up-bow")
        , (DownBow, "down-bow")
        , (Harmonic, "harmonic")
        , (OpenString, "openstring")
        , (ThumbPosition, "thumb-position")
        , (Fingering, "fingering")
        , (Pluck, "pluck")
        , (DoubleTongue, "double-tongue")
        , (TripleTongue, "triple-tongue")
        , (Stopped, "stopped")
        , (SnapPizzicato, "snap-pizzicato")
        , (Fret, "fret")
        , (String, "string")
        , (HammerOn, "hammer-on")
        , (PullOff, "pull-off")
        , (Bend, "bend")
        , (Tap, "tap")
        , (Heel, "heel")
        , (Toe, "toe")
        , (Fingernails, "fingernails")
        , (Hole, "hole")
        , (Arrow, "arrow")
        , (Handbell, "handbell")
        ]
    , xpThrow "other technical" -- TODO
    ]

instance ReadMusicXml Articulation where
    readMusicXml = unpickleTree xpArticulation
instance ShowMusicXml Articulation where
    showMusicXml = pickleTree xpArticulation

xpArticulation = 
    xpAlt (\s -> case s of
        OtherArticulation -> 1
        _ -> 0)
    [ xpEmptyNodes 
        [ (Accent, "accent")
        , (StrongAccent, "strong-accent")
        , (Staccato, "staccato")
        , (Tenuto, "tenuto")
        , (DetachedLegato, "detached-legato")
        , (Staccatissimo, "staccatissimo")
        , (Spiccato, "spiccato")
        , (Scoop, "scoop")
        , (Plop, "plop")
        , (Doit, "doit")
        , (Falloff, "falloff")
        , (BreathMark, "breathmark")
        , (Caesura, "caesura")
        , (Stress, "stress")
        , (Unstress, "unstress")
        ]
    , xpThrow "other articulation" -- TODO
    ]


xpAccidental = 
    xpWrapAccidental $
    xpElemNodes "accidental-mark" $ xpContent xpText

-- ----------------------------------------------------------------------------------
-- Directions
-- ----------------------------------------------------------------------------------

instance ReadMusicXml Direction where
    readMusicXml = unpickleTree xpDirection
instance ShowMusicXml Direction where
    showMusicXml = pickleTree xpDirection

xpDirection = 
    xpAlt (\s -> case s of
        Rehearsal _ -> 0
        Segno -> 1
        Words _ -> 2
        Coda -> 3
        Crescendo Start -> 4
        Diminuendo Start -> 4
        Crescendo Stop -> 5
        Diminuendo Stop -> 5
        Dynamics _ -> 6
        Metronome _ _ _ -> 7
        Bracket -> 8
        OtherDirection _ -> 9)
    [ xpDirectionRehearsal, xpDirectionSegno, xpDirectionWords, xpDirectionCoda
    , xpDirectionWedgeStart, xpDirectionWedgeStop
    , xpDirectionDynamics, xpDirectionMetronome, xpDirectionBracket, xpDirectionOther
    ]

xpDirectionRehearsal = 
    xpWrap (Rehearsal, \(Rehearsal str) -> str) $
    xpElemNodes "rehearsal" $ xpContent xpText0

xpDirectionSegno = 
    xpWrap ((`seq` Segno), (`seq` ())) $
    xpElemNodes "segno" xpUnit

xpDirectionWords = 
    xpWrap (Words, \(Words str) -> str) $
    xpElemNodes "words" $ xpContent xpText0

xpDirectionCoda = 
    xpWrap ((`seq` Coda), (`seq` ())) $
    xpElemNodes "coda" xpUnit

xpDirectionWedgeStart = 
    xpWrap (
        \s -> case s of
            "crescendo" -> Crescendo Start
            "diminuendo" -> Diminuendo Start
            _ -> error "bad wedge"
      , \s -> case s of
            Crescendo Start -> "crescendo"
            Diminuendo Start -> "diminuendo"
    ) $
    xpElemAttrs "wedge" $ xpAttr "type" xpText

xpDirectionWedgeStop = xpThrow "wedge stop" -- TODO

xpDirectionDynamics = 
    xpWrap (Dynamics, \(Dynamics dyn) -> dyn) $
    xpElemNodes "dynamics" $ xpDynamics

xpDirectionMetronome = 
    xpWrap ( \(noteVal, dotted, tempo) -> Metronome noteVal dotted tempo
           , \(Metronome noteVal dotted tempo) -> (noteVal, dotted, tempo) ) $
    xpElemNodes "metronome" $
        xpTriple
            (xpWrapNoteVal $ xpElemNodes "beat-unit" $ xpContent xpText)
            (xpAlt (\s -> case s of
                True -> 0
                False -> 1) [xpWrap ((`seq` True), (`seq` ())) $ xpElemNodes "beat-unit-dot" xpUnit, xpLift False])
            (xpWrapTempo $ xpElemNodes "per-minute" $ xpContent xpText)
        

xpDirectionBracket = xpThrow "bracket"

xpDirectionOther = xpThrow "other direction"

-- ----------------------------------------------------------------------------------
-- Lyrics
-- ----------------------------------------------------------------------------------

instance ReadMusicXml Lyric where
    readMusicXml = unpickleTree xpLyric
instance ShowMusicXml Lyric where
    showMusicXml = pickleTree xpLyric

xpLyric = xpThrow "lyric"

-- ----------------------------------------------------------------------------------
-- Basic types
-- ----------------------------------------------------------------------------------

xpDynamics = xpEmptyNodes . map (\d -> (d, map toLower . show $ d)) $ [PPPPPP .. FZ]

xpFermataSign = xpWrapFermata xpText

xpYesNo = xpWrap 
    ( \b -> case b of
        "yes" -> True
        "no" -> False,
      \b -> if b then "yes" else "no") xpPrim

xpWrapBeamType = xpWrapDict 
    [ (BeginBeam, "begin")
    , (ContinueBeam, "continue")
    , (EndBeam, "end")
    , (ForwardHook, "forward-hook")
    , (BackwardHook, "backward-hook")
    ]

xpWrapStartStop = xpWrapStartStopContinueChange
xpWrapStartStopChange = xpWrapStartStopContinueChange
xpWrapStartStopContinue = xpWrapStartStopContinueChange

xpWrapStartStopContinueChange = xpWrapDict
    [ (Start, "start")
    , (Stop, "stop")
    , (Continue, "continue")
    , (Change, "change")
    ]

xpWrapStemDirection = xpWrapDict
    [ (StemDown, "down")
    , (StemUp, "up")
    , (StemNone, "none")
    , (StemDouble, "double")
    ]

xpWrapLineType = xpWrapDict
    [ (Solid, "solid")
    , (Dashed, "dashed")
    , (Dotted, "dotted")
    , (Wavy, "wavy")
    ]
    
xpWrapTempo = xpWrap (Tempo . read, \t -> show (round . getTempo $ t :: Int))

xpWrapNoteHead = xpWrapDict
    [ (SlashNoteHead, "slash")
    , (TriangleNoteHead, "triangle")
    , (DiamondNoteHead, "diamond")
    , (SquareNoteHead, "square")
    , (CrossNoteHead, "cross")
    , (XNoteHead, "x")
    , (CircleXNoteHead, "circle")
    , (InvertedTriangleNoteHead, "inverted-triangle")
    , (ArrowDownNoteHead, "arrow-down")
    , (ArrowUpNoteHead, "arrow-up")
    , (SlashedNoteHead, "slashed")
    , (BackSlashedNoteHead, "back-slashed")
    , (NormalNoteHead, "normal")
    , (ClusterNoteHead, "cluster")
    , (CircleDotNoteHead, "circle")
    , (LeftTriangleNoteHead, "left-triangle")
    , (RectangleNoteHead, "rectangle")
    , (NoNoteHead, "none")
    ]

xpWrapAccidental = xpWrapDict
    [ (DoubleFlat, "double-flat")
    , (Flat, "flat")
    , (Natural, "natural")
    , (Sharp, "sharp")
    , (DoubleSharp, "double-sharp")
    ]

xpWrapNoteVal = xpWrapDict
    [ (NoteVal (1/1024), "1024th")
    , (NoteVal (1/512), "512th")
    , (NoteVal (1/256), "256th")
    , (NoteVal (1/128), "128th")
    , (NoteVal (1/64), "64th")
    , (NoteVal (1/32), "32nd")
    , (NoteVal (1/16), "16th")
    , (NoteVal (1/8), "eighth")
    , (NoteVal (1/4), "quarter")
    , (NoteVal (1/2), "half")
    , (NoteVal (1/1), "whole")
    , (NoteVal (2/1), "breve")
    , (NoteVal (4/1), "long")
    , (NoteVal (8/1), "maxima")
    ]

xpWrapClefSign = xpWrapDict
    [ (GClef, "G")
    , (CClef, "C")
    , (FClef, "F")
    , (PercClef, "percussion")
    , (TabClef, "tab")
    ]

xpWrapNoteSize = xpWrapDict
    [ (SizeFull, "full")
    , (SizeCue, "cue")
    , (SizeLarge, "large")
    ]

xpWrapGroupSymbol = xpWrapDict
    [ (GroupBrace, "brace")
    , (GroupLine, "line")
    , (GroupBracket, "bracket")
    , (GroupSquare, "square")
    , (NoGroupSymbol, "none")
    ]

xpWrapGroupBarlines = xpWrapDict
    [ (GroupBarLines, "yes")
    , (GroupNoBarLines, "no")
    , (GroupMensurstrich, "Mensurstrich")
    ]

xpWrapTie = xpWrapStartStop

xpWrapFermata = xpWrapDict 
    [ (NormalFermata, "normal")
    , (AngledFermata, "angled")
    , (SquaredFermata, "squared")
    ]

xpWrapDict :: (Eq a, Eq b) => [(b, a)] -> PU t a -> PU t b
xpWrapDict ps = xpWrap (unpFn, pFn)
    where
        unpFn x = fst . maybe (error "no match") id . find ((== x) . snd) $ ps
        pFn x = snd . maybe (error "no match") id . find ((== x) . fst) $ ps

xpEmptyNodes :: Eq a => [(a,String)] -> PU [UNode String] a
xpEmptyNodes ps = xpAlt indexFn . map xpFn $ ps
    where
        indexFn x = maybe (error "no match") id $ elemIndex x (map fst ps)

        xpFn (x,s) = 
            xpWrap ((`seq` x), (`seq` ())) $
            xpElemNodes s xpUnit
