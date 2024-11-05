{-# LANGUAGE UnicodeSyntax #-}
module Brian.TrifectaPlus
  ( parseTextual
  , readM
  ) where

import Base1T
import Prelude ( error )

-- base --------------------------------

import Data.Typeable ( typeOf )

-- options-applicative -----------------

import Options.Applicative ( ReadM, eitherReader )

-- text --------------------------------

import Data.Text qualified as T

-- textual-plus ------------------------

import TextualPlus ( TextualPlus )

-- trifecta ----------------------------

import Text.Trifecta.Result ( Result(Failure, Success) )

-- trifecta-plus -----------------------

import TrifectaPlus ( eiText, tParse )

--------------------------------------------------------------------------------

parseTextual ∷ ∀ β α η .
                 (TextualPlus β, Printable α, Typeable β, MonadError 𝕋 η) ⇒
                 α → η β
parseTextual (toText → z) =
  let fromParsed (Success a) = a
      -- this function exists solely to provide a hypothetical value to reflect
      -- on
      fromParsed (Failure _) = error "this should never be evaluated"
      parsedZ                = tParse z
      typ                    = typeOf $ fromParsed parsedZ
   in case parsedZ of
        Success a → return a
        Failure e →
          throwError $ [fmt|failed to parse '%t' as '%w': %t|] z typ (eiText e)

readM ∷ (TextualPlus α, Typeable α) ⇒ ReadM α
readM = eitherReader $ first T.unpack ⊳ parseTextual

-- that's all, folks! ----------------------------------------------------------
