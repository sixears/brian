{-# LANGUAGE UnicodeSyntax #-}
module Brian.Entry
  ( Entry
  , actresses
  , description
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
import Text.Parser.Combinators ( sepBy, (<?>) )

-- sqlite-simple -----------------------

import Database.SQLite.Simple ( ToRow(toRow) )

-- tagsoup -----------------------------

import Text.HTML.TagSoup ( Tag, innerText, partitions, (~/=), (~==) )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus                         ( TextualPlus(textual'), parseTextM,
                                             tparse' )
import TextualPlus.Error.TextualParseError ( AsTextualParseError,
                                             throwAsTextualParseError )

-- text --------------------------------

import Data.Text ( intercalate, pack, replace, unpack, unwords, words )

-- word-wrap ---------------------------

import Text.Wrap ( FillStrategy(FillIndent), WrapSettings(fillStrategy),
                   defaultWrapSettings, wrapText )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.Actresses   ( Actresses )
import Brian.BTag        ( BTags )
import Brian.Description ( Description, more )
import Brian.ID          ( ID, toℤ )
import Brian.Medium      ( Medium )
import Brian.Parsers     ( whitespace )
import Brian.Title       ( Title, unTitle )

--------------------------------------------------------------------------------

(≈) ∷ Tag 𝕋 → 𝕊 → 𝔹
(≈) tag t = (~==) tag ("<" ⊕ t ⊕ ">")

(≉) ∷ Tag 𝕋 → 𝕊 → 𝔹
(≉) tag t = (~/=) tag ("<" ⊕ t ⊕ ">")

text ∷ [Tag 𝕋] → 𝕋
text = unwords ∘ words ∘ innerText


data Entry = Entry { _recordNumber :: ID
                   , _title        :: Title
                   , _medium       :: 𝕄 Medium
                   , _actresses    :: Actresses
                   , _tags         :: BTags
                   , _description  :: Description
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
        ҕ t = string (t ⊕ ":") ⋫ whitespace ⋫ textual' ⋪ whitespace ⋪ char '\n'
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

-- that's all, folks! ----------------------------------------------------------
