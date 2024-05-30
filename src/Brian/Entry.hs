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

-- base --------------------------------

import Data.List  ( drop, filter, reverse, takeWhile )
import Data.Maybe ( catMaybes )
import System.IO  ( putStrLn )
import Text.Read  ( readEither )

-- lens --------------------------------

import Control.Lens.Setter ( (<>~) )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens ( (⊩) )

-- sqlite-simple -----------------------

import Database.SQLite.Simple ( ToRow(toRow) )

-- tagsoup -----------------------------

import Text.HTML.TagSoup ( Tag, innerText, partitions, (~/=), (~==) )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus                         ( tparse )
import TextualPlus.Error.TextualParseError ( AsTextualParseError,
                                             TextualParseError,
                                             throwAsTextualParseError )

-- text --------------------------------

import Data.Text ( breakOn, intercalate, splitOn, stripPrefix, unpack, unwords,
                   words )

-- word-wrap ---------------------------

import Text.Wrap ( FillStrategy(FillIndent), WrapSettings(fillStrategy),
                   defaultWrapSettings, wrapText )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.BTag   ( BTags )
import Brian.ID     ( ID, toℤ )
import Brian.Medium ( Medium )

--------------------------------------------------------------------------------

data Entry = Entry { _recordNumber :: ID
                   , _title        :: 𝕄 𝕋
                   , _medium       :: 𝕄 Medium
                   , _actresses    :: [𝕋]
                   , _tags         :: BTags
                   , _description  :: [𝕋]
                   }
  deriving (Show)

instance ToRow Entry where
  toRow e = toRow (e ⊣ recordNumber, e ⊣ title)

recordNumber ∷ Lens' Entry ID
recordNumber = lens _recordNumber (\ e n → e { _recordNumber = n })

title ∷ Lens' Entry (𝕄 𝕋)
title = lens _title (\ e mt → e { _title = mt })

medium ∷ Lens' Entry (𝕄 Medium)
medium = lens _medium (\ e mm → e { _medium = mm })

actresses ∷ Lens' Entry ([𝕋])
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
                 , [fmt|Title       : %t|] ⊳ (e ⊣ title)
                 , [fmt|Medium      : %T|] ⊳ (e ⊣ medium)
                 , mfmt (e ⊣ actresses) [fmtT|Actresses   : %L|]
                 , mfmt (e ⊣ tags)      [fmt|Tags        : %T|]
                 , mfmt (e ⊣ description)
                        ([fmt|Description :\n  %t|] ∘ wrap ∘ unwords ∘ reverse)
                 ]
    in P.text $ intercalate "\n" (catMaybes fields)

mkEntry ∷ ID → Entry
mkEntry n = Entry { _recordNumber = n, _title = 𝕹, _medium = 𝕹
                  , _actresses = [], _description = [], _tags = ф }

addEntryField ∷ (MonadError ε η, AsTextualParseError ε) ⇒ Entry → 𝕋 → η Entry
addEntryField e t = do
  let p = second (stripPrefix ": ") $ (breakOn ":") t
  x ← case p of
        ("Tags"       , 𝕵 t') → tparse t' ≫ return ∘ (e &) ∘ (tags <>~)
        ("Medium"     , 𝕵 t') → tparse t' ≫ return ∘ (e &) . (medium ⊩)
        ("Actress"    , 𝕵 t') → return $ e & actresses <>~ (splitOn ", " t')
        ("Title"      , 𝕵 t') → return $ e & title       ⊩ t'
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
entryParagraphs p = filter (≢ "") $ text ⊳⊳ partitions (≈ "br")
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

parseEntries ∷ (AsTextualParseError ε, MonadError ε η) ⇒ [Tag 𝕋] → η [Entry]
parseEntries ts = mapM parseEntry (partitions (≈ "blockquote") ts)

printEntry ∷ MonadIO μ ⇒ Entry → μ ()
printEntry ts = liftIO ∘ putStrLn $ [fmt|%T\n|] ts

-- that's all, folks! ----------------------------------------------------------
