{-# LANGUAGE UnicodeSyntax #-}
module Brian.BTag
  ( BTag
  , BTags(unBTags)
  ) where

import Base1

import Prelude ( undefined )

-- parsers -----------------------------

import Text.Parser.Char        ( oneOf, spaces, string )
import Text.Parser.Combinators ( sepBy, (<?>) )
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
  textual' = BTag ∘ pack ⊳ many (oneOf ('_' : ['a'..'z'])) <?> "BTag"

instance ToField BTag where
  toField = toField ∘ unBTag

instance FromField BTag where
  fromField f = case fromField f of
    Ok t     → Ok $ BTag t
    Errors x → Errors x

newtype BTags = BTags { unBTags :: [BTag] }

instance TextualPlus BTags where
  textual' = (BTags ⊳ textual' `sepBy` (string "," ⋪ spaces)) <?> "BTags"

-- that's all, folks! ----------------------------------------------------------
