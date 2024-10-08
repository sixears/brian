{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE UnicodeSyntax   #-}
module Brian.Entry
  ( Entry(Entry)
  , EntryRow
  , actresses
  , description
  , entryRow
  , episode
  , medium
  , parseEntries
  , printEntry
  , recordNumber
  , tags
  , title
  ) where

import Base1T

-- base --------------------------------

import Control.Applicative ( Alternative )
import Control.Monad.Fail  ( MonadFail )
import Data.Either         ( partitionEithers )
import Data.List           ( filter, takeWhile )
import Data.Maybe          ( catMaybes, fromMaybe )
import System.IO           ( putStrLn )

-- lens --------------------------------

import Control.Lens.Getter ( view )

-- natural -----------------------------

import Natural ( length )

-- parsers -----------------------------

import Text.Parser.Char        ( CharParsing, char, noneOf, string )
import Text.Parser.Combinators ( eof, sepBy, (<?>) )

-- sqlite-simple -----------------------

import Database.SQLite.Simple         ( ToRow(toRow) )
import Database.SQLite.Simple.ToField ( ToField(toField) )

-- tagsoup -----------------------------

import Text.HTML.TagSoup ( Tag, partitions )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit ( assertFailure )

-- tasty-plus --------------------------

import TastyPlus ( assertListEq )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus                         ( TextualPlus(textual'), parseTextM,
                                             tparse' )
import TextualPlus.Error.TextualParseError ( AsTextualParseError,
                                             throwAsTextualParseError )

-- text --------------------------------

import Data.Text qualified as T

-- textual-plus ------------------------

import TextualPlus                         ( parseText )
import TextualPlus.Error.TextualParseError ( tparseToME' )

-- time --------------------------------

import Data.Time.Clock          ( getCurrentTime, utctDay )
import Data.Time.Format.ISO8601 ( iso8601ParseM, iso8601Show )

-- trifecta ----------------------------

import Text.Trifecta.Result ( Result(Failure, Success) )

-- trifecta-plus -----------------------

import TrifectaPlus ( tParse )

-- word-wrap ---------------------------

import Text.Wrap ( FillStrategy(FillIndent), WrapSettings(fillStrategy),
                   defaultWrapSettings, wrapText )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.Description qualified as Description

import Brian.Actress     ( Actresses )
import Brian.BTag        ( BTags )
import Brian.Day         ( Day )
import Brian.Description ( Description(Description, unDescription), more )
import Brian.Episode     ( Episode, EpisodeID(EpisodeID), EpisodeName, epID,
                           epName, mkEpisode )
import Brian.ID          ( ID(ID), toℤ )
import Brian.Medium      ( Medium(Movie, SoapOpera, TVSeries) )
import Brian.Parsers     ( whitespace )
import Brian.TagSoup     ( text, (≈), (≉) )
import Brian.Title       ( Title(Title), unTitle )

--------------------------------------------------------------------------------

data Entry = Entry { _recordNumber :: ID
                   , _title        :: Title
                   , _medium       :: 𝕄 Medium
                   , _actresses    :: Actresses
                   , _tags         :: BTags
                   , _description  :: Description
                   , _episode      :: 𝕄 Episode
                   , _entryDate    :: Day
                   }
  deriving (Eq, Show)

recordNumber ∷ Lens' Entry ID
recordNumber = lens _recordNumber (\ e n → e { _recordNumber = n })

title ∷ Lens' Entry Title
title = lens _title (\ e t → e { _title = t })

medium ∷ Lens' Entry (𝕄 Medium)
medium = lens _medium (\ e mm → e { _medium = mm })

actresses ∷ Lens' Entry Actresses
actresses = lens _actresses (\ e as → e { _actresses = as })

tags ∷ Lens' Entry BTags
tags = lens _tags (\ e as → e { _tags = as })

description ∷ Lens' Entry Description
description = lens _description (\ e d → e { _description = d })

episode ∷ Lens' Entry (𝕄 Episode)
episode = lens _episode (\ e d → e { _episode = d })

entryDate ∷ Lens' Entry Day
entryDate = lens _entryDate (\ e d → e { _entryDate = d })

----------------------------------------

instance Printable Entry where
  print e =
    let wd = 80
        mfmt xs f = case xs of [] → 𝕹; _ →  𝕵 $ f xs
        wrapn i = wrapText defaultWrapSettings { fillStrategy=FillIndent i }
                           (fromIntegral wd)
        fields = [ 𝕵 $ [fmt|Record      : %06d|] (toℤ $ e ⊣ recordNumber)
                 , 𝕵 $ [fmt|EntryDate   : %T|] (e ⊣ entryDate)
                 , 𝕵 $ [fmt|Title       : %t|] (unTitle $ e ⊣ title)
                 , [fmt|Medium      : %T|] ⊳ (e ⊣ medium)
                 , [fmt|Episode     : %T|] ⊳ (toText ⊳ e ⊣ episode)
                 , 𝕵 $ [fmtT|Actresses   : %T|]  (e ⊣ actresses)
                 , mfmt (wrapn 14 ∘ toText $ e ⊣ tags) [fmt|Tags        : %t|]
                 , let descn = toText $ e ⊣ description
                   in  𝕵 $ [fmtT|Description : %t|]
                       (if length descn + 14 ≤ wd
                        then descn
                        else "\n  " ⊕ wrapn 2 (T.replace "\n" "\n\n  " descn))
                 ]
    in P.text $ T.intercalate "\n" (catMaybes fields)

------------------------------------------------------------

data EntryRow = EntryRow { _erRecordNumber :: ID
                         , _erTitle        :: Title
                         , _erMedium       :: 𝕄 Medium
                         , _arActresses    :: Actresses
                         , _erDescription  :: Description
                         , _erEpisodeID    :: EpisodeID
                         , _erEpisodeName  :: 𝕄 EpisodeName
                         , _erEntryDate    :: Day
                         }
  deriving (Show)

entryRow ∷ Day → Entry → EntryRow
entryRow d e = EntryRow (e ⊣ recordNumber)
                        (e ⊣ title)
                        (e ⊣ medium)
                        (e ⊣ actresses)
                        (e ⊣ description)
                        (maybe (EpisodeID []) (view epID) (e ⊣ episode))
                        (maybe 𝕹 (view epName) (e ⊣ episode))
                        d

instance ToRow EntryRow where
  toRow (EntryRow rn tt md ac ds epid epn ed) =
    toRow (rn, unTitle tt, md, toField ac, toField ds, toField epid,toField epn,
           toField ed)

----------------------------------------

parseEithers ∷ Alternative ψ ⇒ ψ α → ψ β → ψ sep → ψ ([α], [β])
parseEithers l r n = partitionEithers ⊳ (𝕷 ⊳ l ∤ 𝕽 ⊳ r) `sepBy` n

instance TextualPlus Entry where
  textual' =
    let mkEntry (n,t,m,a,d,(gs,ds)) = do
          tgs ← ю ⊳ mapM (parseTextM "BTag*") gs
          (e,d') ← case tParse @Episode (T.unpack $ unDescription d) of
            Success e → return (𝕵 e, Description.fromLines (T.pack ⊳ ds))
            Failure _ → return (𝕹, d `more` (T.pack ⊳ ds))
          return $ Entry { _recordNumber = n
                         , _title = t
                         , _medium = 𝕵 m
                         , _description = d'
                         , _actresses = a
                         , _tags = tgs
                         , _episode = e
                         }
        ҕ ∷ ∀ α η . (TextualPlus α, MonadFail η, CharParsing η) ⇒ 𝕊 → η α
        ҕ t = let end = (pure () ⋪ char '\n') ∤ eof
              in  string (t ⊕ ":") ⋫ whitespace ⋫ textual' ⋪ whitespace ⋪ end
        restOfLine = many $ noneOf "\n"
    in ((,,,,,) ⊳ ҕ "Record number"
                ⊵ ҕ "Title"
                ⊵ ҕ "Medium"
                ⊵ ҕ "Actress"
                ⊵ ҕ @Description "Description"
                ⊵ parseEithers (T.pack ⊳ (string "Tags: " ⋫ restOfLine))
                               restOfLine (char '\n')
                <?> "Entry") ≫ mkEntry

----------------------------------------

parseEntry ∷ (MonadError ε η, AsTextualParseError ε) ⇒ [𝕋] → η Entry
parseEntry ts =
  case tparse' (T.intercalate "\n" ts) of
    𝕽 e   → return e
    𝕷 err → throwAsTextualParseError "no parse Entry"
                                     (toString err : (T.unpack ⊳ ts))

----------------------------------------

entryParagraphs ∷ [Tag 𝕋] → [𝕋]
entryParagraphs p =
  filter (≢ "") $ text ⊳⊳ (\ ts → takeWhile (≉"br") ts : partitions (≈ "br") ts)
                $ takeWhile (≉ "/blockquote") p

----------------------------------------

parseEntries ∷ (AsTextualParseError ε, MonadError ε η) ⇒ [Tag 𝕋] → η [Entry]
parseEntries ts =
  mapM parseEntry (entryParagraphs ⊳ partitions (≈ "blockquote") ts)

----------------------------------------

printEntry ∷ MonadIO μ ⇒ Entry → μ ()
printEntry ts = liftIO ∘ putStrLn $ [fmt|%T\n|] ts

-- tests -----------------------------------------------------------------------

checkT ∷ 𝕋 → Entry → TestTree
checkT input exp =
  let tname = T.unpack ∘ fromMaybe "--XX--" ∘ head $ T.lines input in
  case (tparseToME' ∘ parseText) input of
    𝕷 e → testCase (tname ⊕ ": parseText") $ assertFailure $ show e
    𝕽 e →
        let tt ∷ ∀ α . (Eq α, Show α) ⇒ TestName → Lens' Entry α → TestTree
            tt nm ln = testCase nm $ exp ⊣ ln @=? e ⊣ ln
        in testGroup tname $
             [ tt "recordNumber" recordNumber
             , tt "title"        title
             , tt "medium"       medium
             , tt "actresses"    actresses
             , tt "tags"         tags
             , tt "episode"      episode
             , assertListEq "description"
                            (T.lines ∘ unDescription $ exp ⊣ description)
                            (T.lines ∘ unDescription $ e ⊣ description)
             ]

{-| unit tests -}
tests ∷ TestTree
tests =
  let unlines = T.intercalate "\n"
  in  testGroup "Entry"
      [ let t = unlines [ "Record number: 1"
                        , "Title: Guiding Light"
                        , "Medium: Soap Opera"
                        , "Actress: Sherry Stringfield"
                        , "Description: Aired December of 1990."
                        , T.unwords [ "Stringfield is kidnapped and held for"
                                    , "ransom by her ex. Tied to a" ]
                        , T.unwords [ "chair and gagged with white cloth"
                                    , "between the teeth. Several good"
                                    , "closeups. Ungagged for a phone call,"
                                    , "then regagged on screen."
                                    ]
                        , T.unwords [ "Tags: country_us, gagtype_cleave,"
                                    , "bonddesc_chair, onscreen_gagging" ]
                        ]
         in checkT t
                   (Entry { _recordNumber = ID 1, _title = "Guiding Light"
                          , _medium = 𝕵 SoapOpera
                          , _actresses = ["Sherry Stringfield"]
                          , _description = Description $
                            unlines [ "Aired December of 1990."
                                    , T.unwords [ "Stringfield is kidnapped"
                                                , "and held for ransom by her"
                                                , "ex. Tied to a" ]
                                    , T.unwords [ "chair and gagged with white"
                                                , "cloth between the teeth."
                                                , "Several good closeups."
                                                , "Ungagged for a phone call,"
                                                , "then regagged on screen."
                                                ]
                                    ]
                          , _tags = [ "country_us", "gagtype_cleave"
                                    , "bonddesc_chair", "onscreen_gagging"]
                          , _episode = 𝕹
                          })
      , let t = unlines [ "Record number: 3"
                        , "Title: The Amazing Spider-Man (1978) aka Spiderman"
                        , "Medium: TV Series"
                        , "Actress: Madeleine Stowe"
                        , T.unwords [ "Description: Episode: \"Escort to"
                                    , "Danger\" (1.06)" ]
                        , T.unwords [ "As a kidnapped foreign"
                                    , "princess, she is kidnapped by"
                                    , "terrorists. She." ]
                        , T.unwords [ "is sitting in a warehouse talking to one"
                                    , "of her captors, and is" ]
                        , T.unwords [ "gagged with white cloth between the"
                                    , "teeth (on screen). Short scene," ]
                        , T.unwords [ "but some pretty good closeups. She is"
                                    , "wearing a purple sleeveless" ]
                        , T.unwords [ "gown." ]
                        , T.unwords [ "Tags: bonddesc_anklestogether,"
                                    , "bonddesc_handsbehind, gagtype_cleave,"
                                    , "onscreen_gagging, onscreen_tying,"
                                    , "outfit_skirt, restraint_rope, country_us"
                                    ]
                        ]
         in checkT t
                   (Entry { _recordNumber = ID 3
                          , _title =
                            Title $ T.unwords [ "The Amazing Spider-Man (1978)"
                                              , "aka Spiderman" ]
                          , _medium = 𝕵 TVSeries
                          , _actresses = ["Madeleine Stowe"]
                          , _description = Description $
                              unlines [ T.unwords
                                          [ "As a kidnapped foreign"
                                          , "princess, she is kidnapped by"
                                          , "terrorists. She."
                                          ]
                                      , T.unwords
                                          [ "is sitting in a warehouse talking"
                                          , "to one of her captors, and is" ]
                                      , T.unwords
                                          [ "gagged with white cloth between"
                                          , "the teeth (on screen). Short"
                                          , "scene,"
                                          ]
                                      , T.unwords
                                          [ "but some pretty good closeups. She"
                                          , "is wearing a purple sleeveless" ]
                                      , T.unwords [ "gown." ]
                                      ]
                          , _tags = [ "bonddesc_anklestogether"
                                    , "bonddesc_handsbehind", "gagtype_cleave"
                                    , "onscreen_gagging", "onscreen_tying"
                                    , "outfit_skirt", "restraint_rope"
                                    , "country_us"]
                          , _episode = 𝕵 (mkEpisode [1,6] (𝕵"Escort to Danger"))
                          })
      , let t = unlines [ "Record number: 158"
                        , "Title: Ninja III: The Domination (1984)"
                        , "Medium: Movie"
                        , "Actress: Lucinda Dickey"
                        , T.unwords [ "Description: About halfway through, she"
                                    , "appears, ungagged, standing bound"
                                    , "between two posts by ropes tied to"
                                    , "leather cuffs around her outstretched"
                                    , "wrists, and by two chains attached to a"
                                    , "belt around her midsection, as she"
                                    , "undergoes a ritual to call up the spirit"
                                    , "of a ninja that has possessed her."
                                    ]
                        ]
          in checkT t
          (Entry { _recordNumber = ID 158
                 , _title = "Ninja III: The Domination (1984)"
                 , _medium = 𝕵 Movie
                 , _actresses = ["Lucinda Dickey"]
                 , _description = Description $
                     T.unwords [ "About halfway through, she appears, ungagged,"
                               , "standing bound between two posts by ropes"
                               , "tied to leather cuffs around her outstretched"
                               , "wrists, and by two chains attached to a belt"
                               , "around her midsection, as she undergoes a"
                               , "ritual to call up the spirit of a ninja that"
                               , "has possessed her."
                               ]
                 , _tags = []
                 , _episode = 𝕹
                 })
        ]

_test ∷ IO ExitCode
_test = runTestTree tests

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
