{-# LANGUAGE UnicodeSyntax #-}
module Brian.PredicateFilter
  ( PredicateFilter(..)
  , ShowablePredicate(predMatch)
    -- , entryMatches
  , matchFilt
  , readM
  ) where

import Base1T  hiding ( toList )
import Prelude ( error )

-- base --------------------------------

import Control.Monad.Fail ( MonadFail )
import Data.Foldable      ( all, and, any )
import Data.List          ( intercalate, zip )
import Data.Typeable      ( typeOf )
import GHC.Exts           ( toList )

-- natural -----------------------------

import Natural ( length )

-- options-applicative -----------------

import Options.Applicative ( ReadM, eitherReader )

-- parsers -----------------------------

import Text.Parser.Char        ( CharParsing, char, string )
import Text.Parser.Combinators ( sepByNonEmpty, try )

-- parser-plus -------------------------

import ParserPlus ( brackets, whitespaces )

-- regex-with-pcre ---------------------

import Text.RE.PCRE.Text ()

-- text --------------------------------

import Data.Text qualified as T

-- textual-plus ------------------------

import TextualPlus ( TextualPlus(textual') )

-- trifecta ----------------------------

import Text.Trifecta.Result ( Result(Failure, Success) )

-- trifecta-plus -----------------------

import TrifectaPlus ( eiText, tParse )

--------------------------------------------------------------------------------

class ShowablePredicate α β | α → β where
  predMatch ∷ α → β → 𝔹

{-| Filter on α, with ability to construct arbitrary conjunctions & disjunctions
    of many filters.
-}
data PredicateFilter α where EF_Conj :: NonEmpty (PredicateFilter α) -> PredicateFilter α
                             EF_Disj :: NonEmpty (PredicateFilter α) -> PredicateFilter α
                             EF_Pred :: α -> PredicateFilter α

--------------------

instance Show α ⇒ Show (PredicateFilter α) where
  show (EF_Pred a)  = show a
  show (EF_Conj xs) = "AND[" ⊕ intercalate "," (show ⊳ toList xs) ⊕ "]"
  show (EF_Disj xs) = "OR[" ⊕ intercalate "," (show ⊳ toList xs) ⊕ "]"

--------------------

instance Eq α ⇒ Eq (PredicateFilter α) where
  EF_Pred a   == EF_Pred a'   = a == a'
  EF_Conj xs  == EF_Conj xs'  = and $
    (length xs ≡ length xs'): [ x ≡ x' | (x,x') ← zip (toList xs) (toList xs') ]
  EF_Disj xs  == EF_Disj xs'  = and $
    (length xs ≡ length xs'): [ x ≡ x' | (x,x') ← zip (toList xs) (toList xs') ]
  _           == _            = 𝕱

----------------------------------------

parseFilts ∷ ∀ α μ . (MonadFail μ, CharParsing μ, TextualPlus α) ⇒
             μ (NonEmpty (PredicateFilter α))
parseFilts =
  let whitespaced p = whitespaces ⋫ p ⋪ whitespaces
      separator     = whitespaced $ char ','
  in  brackets (whitespaced (textual' `sepByNonEmpty` try (separator)))

----------------------------------------

instance TextualPlus α ⇒ TextualPlus (PredicateFilter α) where
  textual' = ((string "⋀" ∤ string "&&") ⋪ whitespaces) ⋫ (EF_Conj ⊳ (whitespaces ⋫ parseFilts))
           ∤ (string "⋁" ∤ string "||") ⋫ (EF_Disj ⊳ parseFilts)
           ∤ EF_Pred ⊳ textual'

----------------------------------------

trifectTextual ∷ ∀ β α η .
                 (TextualPlus β, Printable α, Typeable β, MonadError 𝕋 η) ⇒
                 α → η β
trifectTextual (toText → z) =
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

{-
instance (TextualPlus α, Typeable α) ⇒ OptParser (PredicateFilter α) where
  optParse = argument (eitherReader (first T.unpack ⊳ trifectTextual))
                      (metavar "PREDICATE" ⊕ help "episode filter")
-}

readM ∷ (TextualPlus α, Typeable α) ⇒ ReadM α
readM = eitherReader $ first T.unpack ⊳ trifectTextual

----------------------------------------

matchFilt ∷ ShowablePredicate α β ⇒ PredicateFilter α → β → 𝔹
matchFilt (EF_Pred p)  e = predMatch p e
matchFilt (EF_Conj ps) e = all (\ p -> matchFilt p e) ps
matchFilt (EF_Disj ps) e = any (\ p -> matchFilt p e) ps

-- that's all, folks! ----------------------------------------------------------
