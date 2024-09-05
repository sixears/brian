{-# LANGUAGE UnicodeSyntax #-}
module Brian.Title
  ( Title(Title)
  , unTitle
  ) where

import Base1T

-- base --------------------------------

import GHC.Exts ( IsString )

-- parsers -----------------------------

import Text.Parser.Char        ( noneOf )
import Text.Parser.Combinators ( (<?>) )

-- sqlite-simple -----------------------

import Database.SQLite.Simple.FromField ( FromField(fromField) )
import Database.SQLite.Simple.ToField   ( ToField(toField) )

-- text --------------------------------

import Data.Text ( pack )

-- textual-plus ------------------------

import TextualPlus ( TextualPlus(textual') )

--------------------------------------------------------------------------------

newtype Title = Title { unTitle :: 𝕋 }
  deriving newtype (Eq, IsString, Printable, Show)

instance TextualPlus Title where
  textual' = Title ∘ pack ⊳ many (noneOf "\n") <?> "Title"

instance ToField Title where
  toField = toField ∘ unTitle

instance FromField Title where
  fromField = Title ⩺ fromField

-- that's all, folks! ----------------------------------------------------------
