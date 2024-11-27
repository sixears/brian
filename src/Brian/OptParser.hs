{-# LANGUAGE UnicodeSyntax #-}
module Brian.OptParser
  ( OptMkParser(..)
  , OptParser(..)
  ) where

import Base1T

-- optparse-applicative ----------------

import Options.Applicative ( HasMetavar, HasValue, Mod, Parser, ReadM, argument,
                             option )

--------------------------------------------------------------------------------

class OptMkParser α where
  optMkParse ∷ (HasMetavar γ, HasValue γ) ⇒
               (ReadM α → Mod γ α → Parser α) → Mod γ α → Parser α
  optArg ∷ Parser α
  optArg = optMkParse argument ф
  optOpt ∷ Parser α
  optOpt = optMkParse option ф

class OptParser α where
  optParse ∷ Parser α

-- that's all, folks! ----------------------------------------------------------
