{-# LANGUAGE UnicodeSyntax #-}
module Brian.ID
  ( ID(ID)
  , fromℤ
  , toℤ
  , unID
  ) where

import Base1T

import Prelude ( Enum )

-- base --------------------------------

import Text.Read ( Read(readPrec) )

-- sqlite-simple -----------------------

import Database.SQLite.Simple           ( SQLData(SQLInteger) )
import Database.SQLite.Simple.FromField ( FromField(fromField) )
import Database.SQLite.Simple.Ok        ( Ok(Errors, Ok) )
import Database.SQLite.Simple.ToField   ( ToField(toField) )

-- textual-plus ------------------------

import TextualPlus ( TextualPlus(textual') )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.NumberParsing ( denary )
import Brian.ToSQLData     ( ToSQLData(toSQLData) )

--------------------------------------------------------------------------------

newtype ID = ID { unID :: ℕ }
  deriving (Enum, Eq, Ord, Show)

instance TextualPlus ID where
  textual' = ID ⊳ denary

instance Read ID where
  readPrec = ID ⊳ readPrec

toℤ ∷ ID → ℤ
toℤ = fromIntegral ∘ unID

fromℤ ∷ ℤ → ID
fromℤ = ID ∘ fromIntegral

instance ToField ID where
  toField = toField ∘ toℤ

instance FromField ID where
  fromField f = case fromField @ℤ f of
                  Ok n     → Ok $ fromℤ n
                  Errors x → Errors x

instance ToSQLData ID where
  toSQLData = SQLInteger ∘ fromIntegral ∘ toℤ

-- that's all, folks! ----------------------------------------------------------
