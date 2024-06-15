{-# LANGUAGE UnicodeSyntax #-}
module Brian.Actresses
  ( Actresses(Actresses)
  , unActresses
  ) where

import Base1T

-- base --------------------------------

import GHC.Exts ( toList )

-- parsers -----------------------------

import Text.Parser.Char        ( char, noneOf )
import Text.Parser.Combinators ( sepBy, (<?>) )

-- sqlite-simple -----------------------

import Database.SQLite.Simple.ToField ( ToField(toField) )

-- text --------------------------------

import Data.Text ( intercalate, pack )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus ( TextualPlus(textual') )

--------------------------------------------------------------------------------

newtype Actresses = Actresses { unActresses :: [𝕋] }
  deriving newtype (Eq, Show)

instance IsList Actresses where
  type instance Item Actresses = 𝕋
  fromList = Actresses
  toList = unActresses

instance Printable Actresses where
  print = P.text ∘ intercalate ", " ∘ unActresses

instance TextualPlus Actresses where
  textual' = Actresses ⊳ (pack ⊳ some (noneOf ",\n")) `sepBy` (char ',' ⋪ many (char ' ')) <?> "Actresses"

instance ToField Actresses where
  toField = toField ∘ intercalate ", " ∘ unActresses

-- that's all, folks! ----------------------------------------------------------
