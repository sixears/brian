{-# LANGUAGE UnicodeSyntax #-}
module Brian.Episode
  ( Episode(Episode)
  , EpisodeID(EpisodeID, unEpisodeID)
  , EpisodeName(EpisodeName)
  , epID
  , epName
  , epi
  , mkEpisode
  , tests
  ) where

import Base1T
import Prelude ( error )

-- base ---------------------------------

import Data.List ( reverse )
import Text.Read ( read, readEither )

-- lens --------------------------------

import Control.Lens.Getter ( view )
import Control.Lens.Tuple  ( _1, _2 )

-- parsers -----------------------------

import Text.Parser.Char        ( CharParsing, char, digit, noneOf, spaces,
                                 string )
import Text.Parser.Combinators ( sepBy1 )

-- parser-plus -------------------------

import ParserPlus ( parens, tries )

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

import TextualPlus ( TextualPlus(textual'), checkT )

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

--------------------

instance Printable Episode where
  print e = P.text $ case (toText ⊳ e ⊣ epName, toText $ e ⊣ epID) of
                       (𝕹   , eid) → eid
                       (𝕵 "", eid) → eid
                       (𝕵 en, "" ) → en
                       (𝕵 en, eid) → T.intercalate " - " [eid,en]

--------------------

type Bit   = (ℕ,𝕊)
type Piece = 𝔼 [Bit] 𝕊

instance TextualPlus Episode where
  textual' =
    let piece ∷ CharParsing η ⇒ η Piece
        piece = let readX ∷ 𝕊 → Bit
                    readX s = (read s,s)
                in  tries ((𝕷 ⊳ (spaces ⋫
                                 parens ((readX⊳some digit) `sepBy1` char '.')))
                        :| [ 𝕽 ⊳ some (char ' ')
                          , 𝕽 ⊳ some (noneOf " (\n")
                          , 𝕽 ∘ pure ⊳ char '(' ])

        piece_to_s ∷ Piece → 𝕊
        piece_to_s = either (ю ∘ fmap (view _2)) id
        unPieces ∷ [Piece] → Episode
        unPieces xs =
          case reverse xs of
            𝕷 ep_id : ep_name →
              Episode (𝕵∘EpisodeName∘T.pack $ ю (piece_to_s ⊳ reverse ep_name))
                      (EpisodeID $ view _1 ⊳ ep_id)
            _ → Episode (𝕵∘EpisodeName∘T.pack $ ю (piece_to_s ⊳ xs)) (EpisodeID [])
    in  string "Episode: " ⋫ (unPieces ⊳ some piece)

----------------------------------------

mkEpisode ∷ [ℕ] → 𝕄 𝕋 → Episode

mkEpisode eids en = Episode { _episodeID = EpisodeID eids
                            , _ename     = EpisodeName ⊳ en }

----------------------------------------

epi ∷ EpisodeID → 𝕄 EpisodeName → 𝕄 Episode
epi (EpisodeID []) 𝕹                    = 𝕹
epi (EpisodeID []) (𝕵 (EpisodeName "")) = 𝕹
epi epid           𝕹                    =
  𝕵 $ Episode { _episodeID = epid, _ename = 𝕹 }
epi epid           (𝕵 (EpisodeName "")) =
  𝕵 $ Episode { _episodeID = epid, _ename = 𝕹 }
epi epid           (𝕵 epname)           =
  𝕵 $ Episode { _episodeID = epid, _ename = 𝕵 epname }

-- tests -----------------------------------------------------------------------

{-| unit tests -}
tests ∷ TestTree
tests =
  testGroup "Episode"
  [ checkT "Episode: \"Escort to Danger\" (1.06)" $
           mkEpisode [1,6]  $ 𝕵 "\"Escort to Danger\""
  , checkT "Episode: \"The Specialist\" aka \"The Professionals\" (2.08)" $
           mkEpisode [2,8]  $ 𝕵 "\"The Specialist\" aka \"The Professionals\""
  , checkT "Episode: \"Built to Kill\" Part 2 (7.2)" $
           mkEpisode [7,2]  $ 𝕵 "\"Built to Kill\" Part 2"
  ]

_test ∷ IO ExitCode
_test = runTestTree tests

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
