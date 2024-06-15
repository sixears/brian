{-# LANGUAGE UnicodeSyntax #-}
module Brian.Parsers
  ( isSpace
  , whitespace
  ) where

import Base1T

-- base --------------------------------

import Data.Char qualified

-- parsers -----------------------------

import Text.Parser.Char        ( CharParsing, satisfy )
import Text.Parser.Combinators ( skipMany, (<?>) )

--------------------------------------------------------------------------------

{-| like `Data.Char.isSpace`, but *not* a newline -}
isSpace ∷ ℂ → 𝔹
isSpace c = c ≢ '\n' ∧ Data.Char.isSpace c

{- | Parses a white space character (any character which satisfies 'isSpace');
     /not including newline/ -}
whitespace ∷ CharParsing m ⇒ m ()
whitespace =
  let space = satisfy isSpace <?> "space"
  in  skipMany space <?> "whitespace"
{-# INLINE whitespace #-}

-- that's all, folks! ----------------------------------------------------------
