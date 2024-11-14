{-# LANGUAGE UnicodeSyntax #-}
module Brian.TrifectaPlus
  ( readM
  ) where

import Base1T
import Prelude ( error )

-- base --------------------------------

import Control.Monad.Fail ( MonadFail )
import Data.Typeable      ( typeOf )

-- optparse-applicative ----------------

import Options.Applicative ( ReadM, eitherReader )

-- parsers -----------------------------

import Text.Parser.Char        ( CharParsing )
import Text.Parser.Combinators ( eof )

-- text --------------------------------

import Data.Text qualified as T

-- trifecta ----------------------------

import Text.Trifecta.Parser ( Parser, parseString )
import Text.Trifecta.Result ( Result(Failure, Success) )

-- textual-plus ------------------------

import TextualPlus ( TextualPlus(textual') )

-- trifecta-plus -----------------------

import TrifectaPlus ( eiText, tParse )

--------------------------------------------------------------------------------

parseTextual ∷ ∀ β α η .
                 (TextualPlus β, Printable α, Typeable β, MonadError 𝕋 η) ⇒
                 Parser β → α → η β
parseTextual parser (toString → z) =
  let fromParsed (Success a) = a
      -- this function exists solely to provide a hypothetical value to reflect
      -- on
      fromParsed (Failure _) = error "this should never be evaluated"
      parsedZ                = parseString parser ф z
      typ                    = typeOf $ fromParsed parsedZ
   in case parsedZ of
        Success a → return a
        Failure e →
          throwError $ [fmt|failed to parse '%s' as '%w': %t|] z typ (eiText e)

readM ∷ (TextualPlus α, Typeable α) ⇒ ReadM α
readM = eitherReader $ first T.unpack ⊳ parseTextual (textual' ⋪ eof)

-- that's all, folks! ----------------------------------------------------------
