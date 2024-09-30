{-# LANGUAGE UnicodeSyntax #-}
module Brian.Episode
  ( Episode
  , mkEpisode
  ) where

import Base1T

-- base ---------------------------------

import Control.Applicative ( optional )
import Text.Read           ( read )

-- parsers -----------------------------

import Text.Parser.Char        ( char, digit, string )
import Text.Parser.Combinators ( sepBy1 )

-- parser-plus -------------------------

import ParserPlus ( dQuotedString, parens, whitespaces )

-- sqlite-simple -----------------------

import Database.SQLite.Simple.ToField ( ToField(toField) )

-- text --------------------------------

import Data.Text qualified as T

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus ( TextualPlus(textual') )

--------------------------------------------------------------------------------

newtype EpisodeID = EpisodeID { unEpisodeID :: [ℕ] }
  deriving (Eq, Show)

instance Printable EpisodeID where
  print (EpisodeID ns) = P.text $ T.intercalate "." $ [fmt|%02d|] ⊳ ns

------------------------------------------------------------

newtype EpisodeName = EpisodeName { unEpisodeName :: 𝕋 }
  deriving (Eq, Show)

instance Printable EpisodeName where
  print = P.text ∘ unEpisodeName

------------------------------------------------------------

data Episode = Episode { _ename     :: 𝕄 EpisodeName
                       , _episodeID :: EpisodeID
                       }
  deriving (Eq, Show)

instance TextualPlus Episode where
  textual' =
    let ep_name = optional $ (EpisodeName ∘ T.pack)⊳ dQuotedString ⋪ whitespaces
        ep_id   = parens $ EpisodeID ⊳ (read ⊳ some digit) `sepBy1` (char '.')
    in  string "Episode: " ⋫ (Episode ⊳ ep_name) ⊵ ep_id

instance ToField Episode where
  toField (Episode en eid) = toField $ [fmtT|%T\t%t|] eid (maybe "" toText en)

mkEpisode ∷ [ℕ] → 𝕄 𝕋 → Episode

mkEpisode eids en = Episode { _episodeID = EpisodeID eids
                            , _ename     = EpisodeName ⊳ en }

-- that's all, folks! ----------------------------------------------------------
