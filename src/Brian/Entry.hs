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

-- base --------------------------------

import Data.List  ( drop, filter, maximum, reverse, takeWhile, zip )
import Data.Maybe ( catMaybes, fromMaybe )
import System.IO  ( putStrLn )
import Text.Read  ( readEither )

-- lens --------------------------------

import Control.Lens.Setter ( (<>~) )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens ( (⊩) )

-- sqlite-simple -----------------------

import Database.SQLite.Simple ( Connection, FromRow, NamedParam((:=)),
                                Only(Only), Query, SQLData, ToRow(toRow),
                                executeNamed, execute_, open, queryNamed,
                                query_ )

-- tagsoup -----------------------------

import Text.HTML.TagSoup ( Tag, innerText, parseTags, partitions, (~/=), (~==) )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus                         ( parse, parseText, tparse )
import TextualPlus.Error.TextualParseError ( AsTextualParseError,
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

import Brian.BTag   ( BTag, BTags, unBTags )
import Brian.ID     ( ID(ID, unID), toℤ )
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
    let fields = [ 𝕵 $ [fmt|Record      : %06d|] (toℤ $ e ⊣ recordNumber)
                 , [fmt|Title       : %t|] ⊳ (e ⊣ title)
                 , [fmt|Medium      : %T|] ⊳ (e ⊣ medium)
                 , case e ⊣ actresses of
                     [] → 𝕹
                     as → 𝕵 $ [fmt|Actresses   : %L|] as
                 , case e ⊣ tags of
                     [] → 𝕹
                     ts → 𝕵 $ [fmt|Tags        : %T|] ts
                 , case e ⊣ description of
                     [] → 𝕹
                     ts  → 𝕵 $ [fmt|Description :\n  %t|] (wrapText defaultWrapSettings { fillStrategy = FillIndent 2} 80 (unwords $ reverse ts))
                 ]
    in P.text $ intercalate "\n" (catMaybes fields)

mkEntry ∷ ID → Entry
mkEntry n = Entry { _recordNumber = n, _title = 𝕹, _medium = 𝕹
                  , _actresses = [], _description = [], _tags = ф }

addEntryField ∷ (MonadError ε η, AsTextualParseError ε) ⇒ Entry → 𝕋 → η Entry
addEntryField e t = do
  let p = second (stripPrefix ": ") $ (breakOn ":") t
  x ← traceShow ("e",e,"t",t,"p",p) $ case p of
-- CR mpearce
--    ("Tags"       , 𝕵 t') → parseBTags t' ≫ return ∘ (e &) ∘ (tags <>~)
          ("Tags"       , 𝕵 t') → tparse t' ≫ return ∘ (e &) ∘ (tags <>~)
          ("Title"      , 𝕵 t') → return $ e & title       ⊩ t'
          ("Medium"     , 𝕵 t') → tparse t' ≫ return ∘ (e &) . (medium ⊩)
          ("Actress"    , 𝕵 t') → return $ e & actresses <>~ (splitOn ", " t')
          ("Description", 𝕵 t') → return $ e & description ⊧ (t' :)
          (_            , _   ) → return $ e & description ⊧ (t :)
  traceShow ("x",x) $ return x

addEntryFields ∷ (MonadError ε η, AsTextualParseError ε) ⇒ Entry → [𝕋] → η Entry
addEntryFields e ts = foldM addEntryField e ts

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
