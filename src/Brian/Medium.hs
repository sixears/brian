{-# LANGUAGE UnicodeSyntax #-}
module Brian.Medium
  ( Medium(Movie, SoapOpera)
  ) where

import Base1T

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

data Medium = SoapOpera | TVSeries | TVMovie | Movie | MovieSerial | Other deriving
  ( Eq
  , Show
  )

instance Printable Medium where
  print SoapOpera   = P.text "Soap Opera"
  print TVSeries    = P.text "TV Series"
  print TVMovie     = P.text "TV Movie"
  print Movie       = P.text "Movie"
  print MovieSerial = P.text "Movie Serial"
  print Other       = P.text "Other"

instance TextualPlus Medium where
  textual' = choice [ string "Soap Opera"   ⋫ pure SoapOpera
                    , string "TV Series"    ⋫ pure TVSeries
                    , string "TV Movie"     ⋫ pure TVMovie
                    , string "Movie Serial" ⋫ pure MovieSerial
                    , string "Movie"        ⋫ pure Movie
                    , string "Other"        ⋫ pure Other
                    , many anyChar ≫ unexpected -- adds unparsed text to emsg
                    ] <?> "Medium"

instance ToField Medium where
  toField m = toField (toText m)

-- that's all, folks! ----------------------------------------------------------
