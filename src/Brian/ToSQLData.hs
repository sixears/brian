{-# LANGUAGE UnicodeSyntax #-}
module Brian.ToSQLData
  ( ToSQLData(..)
  ) where

-- sqlite-simple -----------------------

import Database.SQLite.Simple ( SQLData )

--------------------------------------------------------------------------------

class ToSQLData α where
  toSQLData ∷ α → SQLData

-- that's all, folks! ----------------------------------------------------------
