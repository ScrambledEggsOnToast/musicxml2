
{-# LANGUAGE 
    GeneralizedNewtypeDeriving, 
    StandaloneDeriving, 
    TypeSynonymInstances, 
    FlexibleInstances #-}

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

module Music.MusicXml.Pitch (

        Pitch(..),
        DisplayPitch(..),
        PitchClass(..),
        Semitones(..),
        noSemitones,

        Octaves(..),
        Fifths(..),
        Line(..),

        Mode(..),
        Accidental(..)

  ) where

import Music.Pitch.Literal

type Pitch        = (PitchClass, Maybe Semitones, Octaves)
type DisplayPitch = (PitchClass, Octaves)

data Mode         = Major | Minor 
                  | Dorian | Phrygian | Lydian | Mixolydian 
                  | Aeolian | Ionian | Locrian | NoMode

data Accidental   = DoubleFlat | Flat | Natural | Sharp | DoubleSharp

data PitchClass   = C | D | E | F | G | A | B

newtype Semitones = Semitones { getSemitones :: Double }    -- ^ Semitones, i.e 100 cent
newtype Octaves   = Octaves { getOctaves :: Int }           -- ^ Octaves, i.e. 1200 cent
newtype Fifths    = Fifths { getFifths :: Int }             -- ^ Number of fifths upwards relative to C (i.e. F is -1, G is 1)
newtype Line      = Line { getLine :: Int }                 -- ^ Line number, from bottom (i.e. 1-5)


noSemitones :: Maybe Semitones
noSemitones = Nothing

deriving instance Eq   PitchClass
deriving instance Ord  PitchClass
deriving instance Enum PitchClass
deriving instance Show PitchClass
deriving instance Read PitchClass

deriving instance Eq   Accidental
deriving instance Ord  Accidental
deriving instance Enum Accidental

deriving instance Eq   Mode
deriving instance Ord  Mode
deriving instance Enum Mode
deriving instance Show Mode
deriving instance Read Mode

deriving instance Eq   Semitones
deriving instance Ord  Semitones
deriving instance Num  Semitones
deriving instance Enum Semitones
deriving instance Fractional Semitones

deriving instance Eq   Octaves
deriving instance Ord  Octaves
deriving instance Num  Octaves
deriving instance Enum Octaves
deriving instance Real Octaves
deriving instance Integral Octaves

deriving instance Eq   Fifths
deriving instance Ord  Fifths
deriving instance Num  Fifths
deriving instance Enum Fifths

deriving instance Eq   Line
deriving instance Ord  Line
deriving instance Num  Line
deriving instance Enum Line

instance IsPitch Pitch where
    fromPitch (PitchL (pc, Nothing, oct)) = (toEnum pc, Nothing, fromIntegral oct)
    fromPitch (PitchL (pc, Just st, oct)) = (toEnum pc, Just $ fromRational $ toRational $ st, fromIntegral oct)

instance IsPitch DisplayPitch where
    fromPitch (PitchL (pc, _, oct)) = (toEnum pc, fromIntegral oct)

instance IsPitch Fifths where
    fromPitch (PitchL (pc, Nothing, _)) = pitchToFifths pc 0
    fromPitch (PitchL (pc, Just ac, _)) = pitchToFifths pc (round ac)

pitchToFifths 1 (-1) = (-4)
pitchToFifths 2 (-1) = (-3)
pitchToFifths 4 (-1) = (-6)
pitchToFifths 5 (-1) = (-5)
pitchToFifths 6 (-1) = (-2)
pitchToFifths 0 0 = 0
pitchToFifths 1 0 = 2
pitchToFifths 2 0 = 4
pitchToFifths 3 0 = (-1)
pitchToFifths 4 0 = 1
pitchToFifths 5 0 = 3
pitchToFifths 6 0 = 5
pitchToFifths 3 1 = 6
