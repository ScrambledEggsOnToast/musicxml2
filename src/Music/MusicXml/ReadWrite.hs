
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

module Music.MusicXml.ReadWrite (
    ReadMusicXml(..),
    ShowMusicXml(..)
  ) where

import Text.XML.Expat.Pickle (UNode)

class ReadMusicXml a where
    readMusicXml :: [UNode String] -> a

class ShowMusicXml a where
    showMusicXml :: a -> [UNode String]
