{-# LANGUAGE UnicodeSyntax #-}
module Brian.NumberParsing
  ( denary
  , natural
  , octenary
  ) where

import Base1T
import Prelude ( (*) )

-- base --------------------------------

import Data.Char ( digitToInt )

-- parsers -----------------------------

import Text.Parser.Char        ( CharParsing, char, digit, hexDigit, octDigit,
                                 oneOf )
import Text.Parser.Combinators ( (<?>) )

--------------------------------------------------------------------------------

{- ripped shamelessy from Text.Parser.Token; but de-tokenified -}

number ∷ CharParsing μ ⇒ ℕ → μ ℂ → μ ℕ
number base baseDigit =
  foldl' (\x d -> base*x + fromInteger (toInteger $ digitToInt d)) 0 ⊳ some baseDigit

denary ∷ CharParsing μ ⇒ μ ℕ
denary = number 10 digit <?> "denary"

-- | Parses a non-negative whole number in the hexadenary system. The number
-- should be prefixed with \"x\" or \"X\". Returns the value of the
-- number.
--
-- This parser does NOT swallow trailing whitespace
hexadenary ∷ CharParsing μ ⇒ μ ℕ
hexadenary = number 16 hexDigit <?> "hexadenary"
{-# INLINE hexadenary #-}

hexadenary' ∷ CharParsing μ ⇒ μ ℕ
hexadenary' = char '0' ⋫ oneOf "xX" ⋫ hexadenary <?> "hexadenary'"

-- | Parses a non-negative whole number in the octenary system. The number
-- should be prefixed with \"o\" or \"O\". Returns the value of the
-- number.
--
-- This parser does NOT swallow trailing whitespace
octenary ∷ CharParsing μ ⇒ μ ℕ
octenary = number 8 octDigit <?> "octenary"
{-# INLINE octenary #-}

octenary' ∷ CharParsing μ ⇒ μ ℕ
octenary' = char '0' ⋫ oneOf "oO" ⋫ octenary' <?> "octenary'"

natural ∷ CharParsing μ ⇒ μ ℕ
natural = (hexadenary' ∤ octenary' ∤ denary) <?> "natural"

-- that's all, folks! ----------------------------------------------------------
