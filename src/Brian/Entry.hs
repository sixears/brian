{-# LANGUAGE UnicodeSyntax #-}
module Brian.Entry
  ( Entry(Entry)
  , actresses
  , description
  , entryTable
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
import Data.Either         ( partitionEithers )
import Data.List           ( filter, takeWhile )
import Data.Maybe          ( catMaybes )
import System.IO           ( putStrLn )

-- parsers -----------------------------

import Text.Parser.Char        ( char, noneOf, string )
import Text.Parser.Combinators ( eof, sepBy, (<?>) )

-- sqlite-simple -----------------------

import Database.SQLite.Simple         ( ToRow(toRow) )
import Database.SQLite.Simple.ToField ( ToField(toField) )

-- tagsoup -----------------------------

import Text.HTML.TagSoup ( Tag, partitions )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus                         ( TextualPlus(textual'), parseTextM,
                                             tparse' )
import TextualPlus.Error.TextualParseError ( AsTextualParseError,
                                             throwAsTextualParseError )

-- text --------------------------------

import Data.Text qualified as Text

import Data.Text ( intercalate, pack, replace, unpack, unwords )

-- textual-plus ------------------------

import TextualPlus                         ( parseText )
import TextualPlus.Error.TextualParseError ( tparseToME' )

-- word-wrap ---------------------------

import Text.Wrap ( FillStrategy(FillIndent), WrapSettings(fillStrategy),
                   defaultWrapSettings, wrapText )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.Actresses   ( Actresses )
import Brian.BTag        ( BTags )
import Brian.Description ( Description(Description), more )
import Brian.ID          ( ID(ID), toℤ )
import Brian.Medium      ( Medium(Movie, SoapOpera) )
import Brian.Parsers     ( whitespace )
import Brian.SQLite      ( Column(Column), ColumnFlag(PrimaryKey),
                           ColumnType(CTypeInteger, CTypeText), Table(Table),
                           TableFlag(OkayIfExists) )
import Brian.TagSoup     ( text, (≈), (≉) )
import Brian.Title       ( Title, unTitle )

--------------------------------------------------------------------------------

entryTable ∷ Table
entryTable = Table "Entry" [ OkayIfExists ]
         [ Column "id"          CTypeInteger [PrimaryKey]
         , Column "title"       CTypeText    ф
         , Column "medium"      CTypeText    ф
         , Column "actresses"   CTypeText    ф
         , Column "description" CTypeText    ф
         ]

data Entry = Entry { _recordNumber :: ID
                   , _title        :: Title
                   , _medium       :: 𝕄 Medium
                   , _actresses    :: Actresses
                   , _tags         :: BTags
                   , _description  :: Description
                   }
  deriving (Eq, Show)

instance ToRow Entry where
  toRow e = toRow ( e ⊣ recordNumber
                  , unTitle $ e ⊣ title
                  , e ⊣ medium
                  , toField (e ⊣ actresses)
                  , toField (e ⊣ description)
                  )

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

instance Printable Entry where
  print e =
    let mfmt xs f = case xs of [] → 𝕹; _ →  𝕵 $ f xs
        wrap = wrapText defaultWrapSettings { fillStrategy = FillIndent 2 } 80
        fields = [ 𝕵 $ [fmt|Record      : %06d|] (toℤ $ e ⊣ recordNumber)
                 , 𝕵 $ [fmt|Title       : %t|] (unTitle $ e ⊣ title)
                 , [fmt|Medium      : %T|] ⊳ (e ⊣ medium)
                 , 𝕵 $ [fmtT|Actresses   : %T|]  (e ⊣ actresses)
                 , mfmt (e ⊣ tags)      [fmt|Tags        : %T|]
                 , 𝕵 $ [fmtT|Description : %t|]
                       (wrap ∘ replace "\n" "\n\n  " ∘ toText $ e ⊣ description)
                 ]
    in P.text $ intercalate "\n" (catMaybes fields)

entryParagraphs ∷ [Tag 𝕋] → [𝕋]
entryParagraphs p =
  filter (≢ "") $ text ⊳⊳ (\ ts → takeWhile (≉"br") ts : partitions (≈ "br") ts)
                $ takeWhile (≉ "/blockquote") p

parseEithers ∷ Alternative ψ ⇒ ψ α → ψ β → ψ sep → ψ ([α], [β])
parseEithers l r n = partitionEithers ⊳ (𝕷 ⊳ l ∤ 𝕽 ⊳ r) `sepBy` n

instance TextualPlus Entry where
  textual' =
    let
        mkEntry (n,t,m,a,d,(gs,ds)) = do
          tgs ← ю ⊳ mapM (parseTextM "BTag*") gs
          return $ Entry { _recordNumber = n
                         , _title = t
                         , _medium = 𝕵 m
                         , _description = d `more` (pack ⊳ ds)
                         , _actresses = a
                         , _tags = tgs
                         }
        ҕ t = let end = (pure () ⋪ char '\n') ∤ eof
              in  string (t ⊕ ":") ⋫ whitespace ⋫ textual' ⋪ whitespace ⋪ end
        restOfLine = many $ noneOf "\n"
    in ((,,,,,) ⊳ ҕ "Record number"
                ⊵ ҕ "Title"
                ⊵ ҕ "Medium"
                ⊵ ҕ "Actress"
                ⊵ ҕ "Description"
                ⊵ parseEithers (pack ⊳ (string "Tags: " ⋫ restOfLine))
                               restOfLine (char '\n')
                <?> "Entry") ≫ mkEntry

parseEntry ∷ (MonadError ε η, AsTextualParseError ε) ⇒ [𝕋] → η Entry
parseEntry ts =
  case tparse' (intercalate "\n" ts) of
    𝕽 e   → return e
    𝕷 err → throwAsTextualParseError "no parse Entry"
                                     (toString err : (unpack ⊳ ts))

parseEntries ∷ (AsTextualParseError ε, MonadError ε η) ⇒ [Tag 𝕋] → η [Entry]
parseEntries ts =
  mapM parseEntry (entryParagraphs ⊳ partitions (≈ "blockquote") ts)

printEntry ∷ MonadIO μ ⇒ Entry → μ ()
printEntry ts = liftIO ∘ putStrLn $ [fmt|%T\n|] ts

-- tests -----------------------------------------------------------------------

checkT ∷ (TextualPlus α, Eq α, Show α) ⇒ 𝕋 → α → TestTree
checkT input exp =
  testCase ("parseText: " ⊕ (unpack $ Text.takeWhile (≢ '\n') input)) $
    𝕽 exp @=? (tparseToME' ∘ parseText) input

{-| unit tests -}
tests ∷ TestTree
tests =
  let unlines = intercalate "\n"
  in  testGroup "Entry"
      [ let t = unlines [ "Record number: 1"
                        , "Title: Guiding Light"
                        , "Medium: Soap Opera"
                        , "Actress: Sherry Stringfield"
                        , "Description: Aired December of 1990."
                        , unwords [ "Stringfield is kidnapped and held for"
                                  , "ransom by her ex. Tied to a" ]
                        , unwords [ "chair and gagged with white cloth between"
                                  , "the teeth. Several good closeups. Ungagged"
                                  , "for a phone call, then regagged on screen."
                                  ]
                        , unwords [ "Tags: country_us, gagtype_cleave,"
                                  , "bonddesc_chair, onscreen_gagging" ]
                        ]
          in checkT t
          (Entry { _recordNumber = ID 1, _title = "Guiding Light"
                 , _medium = 𝕵 SoapOpera, _actresses = ["Sherry Stringfield"]
                 , _description = Description $
                   unlines [ "Aired December of 1990."
                           , unwords [ "Stringfield is kidnapped and held for"
                                     , "ransom by her ex. Tied to a" ]
                           , unwords [ "chair and gagged with white cloth"
                                     , "between the teeth. Several good"
                                     , "closeups. Ungagged for a phone call,"
                                     , "then regagged on screen."
                                     ]
                           ]
                 , _tags = [ "country_us", "gagtype_cleave", "bonddesc_chair"
                           , "onscreen_gagging"]
                 })
      , let t = unlines [ "Record number: 158"
                        , "Title: Ninja III: The Domination (1984)"
                        , "Medium: Movie"
                        , "Actress: Lucinda Dickey"
                        , unwords [ "Description: About halfway through, she"
                                  , "appears, ungagged, standing bound between"
                                  , "two posts by ropes tied to leather cuffs"
                                  , "around her outstretched wrists, and by two"
                                  , "chains attached to a belt around her"
                                  , "midsection, as she undergoes a ritual to"
                                  , "call up the spirit of a ninja that has"
                                  , "possessed her." ] ]
          in checkT t
          (Entry { _recordNumber = ID 158
                 , _title = "Ninja III: The Domination (1984)"
                 , _medium = 𝕵 Movie
                 , _actresses = ["Lucinda Dickey"]
                 , _description = Description $
                     unwords [ "About halfway through, she appears, ungagged,"
                             , "standing bound between two posts by ropes tied"
                             , "to leather cuffs around her outstretched"
                             , "wrists, and by two chains attached to a belt"
                             , "around her midsection, as she undergoes a"
                             , "ritual to call up the spirit of a ninja that"
                             , "has possessed her."
                             ]
                 , _tags = []
                 })
        ]

_test ∷ IO ExitCode
_test = runTestTree tests

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
