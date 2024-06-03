{-# LANGUAGE UnicodeSyntax #-}
module Brian.Entry
  ( Entry
  , actresses
  , description
  , medium
  , parseEntries
  , parseEntry
  , printEntry
  , recordNumber
  , tags
  , title
  ) where

import Base1
import Debug.Trace ( traceShow )
import Prelude     ( undefined )

-- base --------------------------------

import Data.Char qualified

import Data.List  ( drop, filter, reverse, takeWhile )
import Data.Maybe ( catMaybes )
import System.IO  ( putStrLn )
import Text.Read  ( read, readEither )

-- lens --------------------------------

import Control.Lens.Setter ( (<>~) )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens ( (⊩) )

-- parsers -----------------------------

import Text.Parser.Char        ( CharParsing, anyChar, char, digit, noneOf,
                                 oneOf, satisfy, spaces, string )
import Text.Parser.Combinators ( sepBy, skipMany, (<?>) )

-- sqlite-simple -----------------------

import Database.SQLite.Simple ( ToRow(toRow) )

-- tagsoup -----------------------------

import Text.HTML.TagSoup ( Tag, innerText, partitions, (~/=), (~==) )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus                         ( TextualPlus(textual'), tparse,
                                             tparse' )
import TextualPlus.Error.TextualParseError ( AsTextualParseError,
                                             TextualParseError,
                                             throwAsTextualParseError )

-- text --------------------------------

import Data.Text ( breakOn, intercalate, pack, splitOn, stripPrefix, unpack,
                   unwords, words )

-- word-wrap ---------------------------

import Text.Wrap ( FillStrategy(FillIndent), WrapSettings(fillStrategy),
                   defaultWrapSettings, wrapText )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.Actresses ( Actresses, unActresses )
import Brian.BTag      ( BTags )
import Brian.ID        ( ID, fromℤ, toℤ )
import Brian.Medium    ( Medium )
import Brian.Title     ( Title(Title), unTitle )

--------------------------------------------------------------------------------

data Entry = Entry { _recordNumber :: ID
                   , _title        :: Title
                   , _medium       :: 𝕄 Medium
                   , _actresses    :: Actresses
                   , _tags         :: BTags
                   , _description  :: [𝕋]
                   }
  deriving (Show)

instance ToRow Entry where
  toRow e = toRow (e ⊣ recordNumber, unTitle $ e ⊣ title)

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

description ∷ Lens' Entry ([𝕋])
description = lens _description (\ e as → e { _description = as })

instance Printable Entry where
  print e =
    let mfmt xs f = case xs of [] → 𝕹; _ →  𝕵 $ f xs
        wrap = wrapText defaultWrapSettings { fillStrategy = FillIndent 2 } 80
        fields = [ 𝕵 $ [fmt|Record      : %06d|] (toℤ $ e ⊣ recordNumber)
                 , 𝕵 $ [fmt|Title       : %t|] (unTitle $ e ⊣ title)
                 , [fmt|Medium      : %T|] ⊳ (e ⊣ medium)
                 , mfmt (unActresses $ e ⊣ actresses) [fmtT|Actresses   : %L|]
                 , mfmt (e ⊣ tags)      [fmt|Tags        : %T|]
                 , mfmt (e ⊣ description)
                        ([fmt|Description :\n  %t|] ∘ wrap ∘ unwords ∘ reverse)
                 ]
    in P.text $ intercalate "\n" (catMaybes fields)

mkEntry ∷ ID → Entry
mkEntry n = Entry { _recordNumber = n, _title = Title "", _medium = 𝕹
                  -- , _actresses = []
                  , _description = [], _tags = ф }

addEntryField ∷ (MonadError ε η, AsTextualParseError ε) ⇒ Entry → 𝕋 → η Entry
addEntryField e t = do
  let p = second (stripPrefix ": ") $ (breakOn ":") t
  x ← case p of
        ("Tags"       , 𝕵 t') → tparse t' ≫ return ∘ (e &) ∘ (tags <>~)
        ("Medium"     , 𝕵 t') → tparse t' ≫ return ∘ (e &) . (medium ⊩)
--        ("Actress"    , 𝕵 t') → return $ e & actresses <>~ (splitOn ", " t')
        ("Title"      , 𝕵 t') → return $ e & title       ⊢ Title t'
        ("Description", 𝕵 t') → return $ e & description ⊧ (t' :)
        (_            , _   ) → return $ e & description ⊧ (t :)
  return x

addEntryField' ∷ (MonadError ε η, AsTextualParseError ε) ⇒ Entry → 𝕋 → η Entry
addEntryField' e t =
  case addEntryField @TextualParseError e t of
    𝕽 x   → return x
    𝕷 err → throwAsTextualParseError ([fmt|failed to parse entry field:%t|] t)
                                     ["«" ⊕ toString err ⊕ "»"]

addEntryFields ∷ (MonadError ε η, AsTextualParseError ε) ⇒ Entry → [𝕋] → η Entry
addEntryFields e ts = foldM addEntryField' e ts

entryParagraphs ∷ [Tag 𝕋] → [𝕋]
entryParagraphs p =
  filter (≢ "") $ text ⊳⊳ (\ ts → takeWhile (≉ "br") ts : partitions (≈ "br") ts)
                $ takeWhile (≉ "/blockquote") p

(≈) ∷ Tag 𝕋 → 𝕊 → 𝔹
(≈) tag t = (~==) tag ("<" ⊕ t ⊕ ">")

(≉) ∷ Tag 𝕋 → 𝕊 → 𝔹
(≉) tag t = (~/=) tag ("<" ⊕ t ⊕ ">")

text ∷ [Tag 𝕋] → 𝕋
text = unwords ∘ words ∘ innerText

parseEntry ∷ (MonadError ε η, AsTextualParseError ε) ⇒ [Tag 𝕋] → η Entry
parseEntry ts =
  case breakOn ": " ⊳ (text ∘ pure ⊳ ts !! 1) of
    𝕵 ("Record number", n) →
      case readEither (drop 2 $ unpack n) of
        𝕷 err → throwAsTextualParseError "unparsed record number"
                                         [err, drop 2 (unpack n)]
        𝕽 n'  → addEntryFields (mkEntry n') (entryParagraphs ts)
    _ → throwAsTextualParseError "no record number!\n" (show ⊳ ts)

isSpace ∷ ℂ → 𝔹
isSpace c = c ≢ '\n' ∧ Data.Char.isSpace c

{- | Parses a white space character (any character which satisfies 'isSpace');
     /not including newline/ -}
whitespace ∷ CharParsing m ⇒ m ()
whitespace =
  let space = satisfy isSpace <?> "space"
  in  skipMany space <?> "whitespace"
{-# INLINE whitespace #-}

instance TextualPlus Entry where
  textual' =
    let mkEntry' ∷ ID → Title → Medium → Actresses → 𝕋 → Entry
        mkEntry' n t m a d = (mkEntry n) { _title = t, _medium = 𝕵 m, _description = [d], _actresses = a }
        ҕ t = string (t ⊕ ":") ⋫ whitespace ⋫ textual' ⋪ whitespace ⋪ char '\n'
    in mkEntry' ⊳ ҕ "Record number"
                ⊵ ҕ "Title"
                ⊵ ҕ "Medium"
                ⊵ ҕ "Actress"
                ⊵ (pack ⊳ many anyChar) <?> "Entry"

parseEntry' ∷ (MonadError ε η, AsTextualParseError ε) ⇒ [𝕋] → η Entry
parseEntry' ts =
  case tparse' (intercalate "\n" ts) of
    𝕽 e  → return e
    𝕷 err → throwAsTextualParseError "no parse Entry" (toString err : (unpack ⊳ ts))

parseEntries ∷ (AsTextualParseError ε, MonadError ε η) ⇒ [Tag 𝕋] → η [Entry]
parseEntries ts =
  mapM parseEntry' (entryParagraphs ⊳ partitions (≈ "blockquote") ts)

printEntry ∷ MonadIO μ ⇒ Entry → μ ()
printEntry ts = liftIO ∘ putStrLn $ [fmt|%T\n|] ts

-- that's all, folks! ----------------------------------------------------------
