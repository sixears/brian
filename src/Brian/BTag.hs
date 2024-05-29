{-# LANGUAGE UnicodeSyntax #-}
module Brian.BTag
  ( BTag
  , BTags(unBTags)
  ) where

import Base1

-- base --------------------------------

import Data.Monoid    ( Monoid(mempty) )
import Data.Semigroup ( Semigroup )

-- parsers -----------------------------

import Text.Parser.Char        ( anyChar, oneOf, spaces, string )
import Text.Parser.Combinators ( choice, sepBy, try, unexpected, (<?>) )
import Text.Parser.Token       ( symbol )

-- sqlite-simple -----------------------

import Database.SQLite.Simple.FromField ( FromField(fromField) )
import Database.SQLite.Simple.Ok        ( Ok(Errors, Ok) )
import Database.SQLite.Simple.ToField   ( ToField(toField) )

-- text --------------------------------

import Data.Text ( pack, splitOn )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus                         ( TextualPlus(textual'), tparse' )
import TextualPlus.Error.TextualParseError ( AsTextualParseError,
                                             throwAsTextualParseError )

--------------------------------------------------------------------------------

newtype BTag = BTag { unBTag :: 𝕋 }
  deriving (Eq, Ord, Show)

instance Printable BTag where
  print = P.text ∘ unBTag

instance TextualPlus BTag where
  textual' = let chars = "_/" ⊕ ['a'..'z'] ⊕ ['A'..'Z']
             in  BTag ∘ pack ⊳ many (oneOf chars) <?> "BTag"

instance ToField BTag where
  toField = toField ∘ unBTag

instance FromField BTag where
  fromField f = case fromField f of
    Ok t     → Ok $ BTag t
    Errors x → Errors x

newtype BTags = BTags { unBTags :: [BTag] }
  deriving (IsList, Printable, Show)
  deriving newtype (Monoid, Semigroup)

instance TextualPlus BTags where
  textual' = (BTags ⊳ textual' `sepBy` (string "," ⋪ spaces)) <?> "BTags"

-- that's all, folks! ----------------------------------------------------------
