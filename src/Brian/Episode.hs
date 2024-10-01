{-# LANGUAGE UnicodeSyntax #-}
module Brian.Episode
  ( Episode(Episode)
  , EpisodeID(EpisodeID)
  , EpisodeName(EpisodeName)
  , epID
  , epName
  , epi
  , mkEpisode
  ) where

import Base1T
import Prelude ( error )

-- base ---------------------------------

import Control.Applicative ( optional )
import Data.Maybe          ( fromMaybe )
import Text.Read           ( readEither )

-- parsers -----------------------------

import Text.Parser.Char        ( char, digit, string )
import Text.Parser.Combinators ( sepBy1 )

-- parser-plus -------------------------

import ParserPlus ( dQuotedString, parens, whitespaces )

-- split -------------------------------

import Data.List.Split ( splitOn )

-- sqlite-simple -----------------------

import Database.SQLite.Simple.FromField ( FromField(fromField) )
import Database.SQLite.Simple.Ok        ( Ok(Errors, Ok) )
import Database.SQLite.Simple.ToField   ( ToField(toField) )

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

instance ToField EpisodeID where
  toField = toField ∘ T.intercalate "." ∘ fmap [fmt|%d|] ∘ unEpisodeID

instance FromField EpisodeID where
  fromField f =
    case fromField f of
      Ok     𝕹      → Ok $ EpisodeID []
      Ok     (𝕵 "") → Ok $ EpisodeID []
      Errors e      → Errors e
      Ok     (𝕵 s)  →
        let readN t =
              case readEither t of
                𝕷 e → error$ [fmt|failed to read '%s' as ℕ (EpisodeID): %T|] t e
                𝕽 r → r
        in  Ok ∘ EpisodeID $ readN ⊳ splitOn "." s

------------------------------------------------------------

newtype EpisodeName = EpisodeName { unEpisodeName :: 𝕋 }
  deriving (Eq, Show)

instance Printable EpisodeName where
  print = P.text ∘ unEpisodeName

instance ToField EpisodeName where
  toField = toField ∘ unEpisodeName

instance FromField EpisodeName where
  fromField = EpisodeName ⩺ fromField

------------------------------------------------------------

data Episode = Episode { _ename     :: 𝕄 EpisodeName
                       , _episodeID :: EpisodeID
                       }
  deriving (Eq, Show)

epName ∷ Lens' Episode (𝕄 EpisodeName)
epName = lens _ename (\ e n → e { _ename = n })

epID ∷ Lens' Episode (EpisodeID)
epID = lens _episodeID (\ e i → e { _episodeID = i })

instance TextualPlus Episode where
  textual' =
    let ep_name = optional $ (EpisodeName ∘ T.pack)⊳ dQuotedString ⋪ whitespaces
        ep_id   =
          let readN s =
                case readEither s of
                  𝕷 e → error$ [fmt|failed to read '%s' as ℕ (Episode): %T|] s e
                  𝕽 r → r
          in  parens $ EpisodeID ⊳ (readN ⊳ some digit) `sepBy1` (char '.')
    in  string "Episode: " ⋫ (Episode ⊳ ep_name) ⊵ ep_id

-- instance ToField Episode where
--   toField (Episode en eid) = toField $ [fmtT|%T\t%t|] eid (maybe "" toText en)

mkEpisode ∷ [ℕ] → 𝕄 𝕋 → Episode

mkEpisode eids en = Episode { _episodeID = EpisodeID eids
                            , _ename     = EpisodeName ⊳ en }

epi ∷ EpisodeID → 𝕄 EpisodeName → 𝕄 Episode
epi (EpisodeID []) 𝕹                    = 𝕹
epi (EpisodeID []) (𝕵 (EpisodeName "")) = 𝕹
epi epid           𝕹                    =
  𝕵 $ Episode { _episodeID = epid, _ename = 𝕹 }
epi epid           (𝕵 (EpisodeName "")) =
  𝕵 $ Episode { _episodeID = epid, _ename = 𝕹 }
epi epid           (𝕵 epname)           =
  𝕵 $ Episode { _episodeID = epid, _ename = 𝕵 epname }

-- that's all, folks! ----------------------------------------------------------
