
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

module Music.MusicXml.Read (
    ReadMusicXml(..)
    
  ) where

import Text.XML.Light (Element)

class ReadMusicXml a where
    readMusicXml :: [Element] -> a

