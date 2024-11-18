{-# LANGUAGE UnicodeSyntax #-}
module Brian.ShowSQL
  ( ShowSQL(..)
  ) where

import Base1T

--------------------------------------------------------------------------------

data ShowSQL = ShowSQL | NoShowSQL deriving (Eq)

-- that's all, folks! ----------------------------------------------------------
