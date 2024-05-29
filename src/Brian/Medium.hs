{-# LANGUAGE UnicodeSyntax #-}
module Brian.Medium
  ( Medium
  ) where

import Base1

-- parsers -----------------------------

import Text.Parser.Char        ( anyChar, string )
import Text.Parser.Combinators ( choice, unexpected, (<?>) )

-- sqlite-simple -----------------------

import Database.SQLite.Simple.ToField ( ToField(toField) )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus ( TextualPlus(textual') )

--------------------------------------------------------------------------------

data Medium = SoapOpera | TVSeries | TVMovie deriving (Show)

instance Printable Medium where
  print SoapOpera = P.text "Soap Opera"
  print TVSeries  = P.text "TV Series"
  print TVMovie   = P.text "TV Movie"

instance TextualPlus Medium where
  textual' = choice [ string "Soap Opera" ⋫ pure SoapOpera
                    , string "TV Series" ⋫ pure TVSeries
                    , string "TV Movie"  ⋫ pure TVMovie
                    , many anyChar ≫ unexpected -- adds unparsed text to emsg
                    ] <?> "Medium"

instance ToField Medium where
  toField m = toField (toText m)

-- that's all, folks! ----------------------------------------------------------
