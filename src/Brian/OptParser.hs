{-# LANGUAGE UnicodeSyntax #-}
module Brian.OptParser
  ( OptParser(..)
  ) where

-- optparse-applicative ----------------

import Options.Applicative ( Parser )

--------------------------------------------------------------------------------

class OptParser α where
  optParse ∷ Parser α

-- that's all, folks! ----------------------------------------------------------
