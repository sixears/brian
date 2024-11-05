{-# LANGUAGE UnicodeSyntax #-}
module Brian.PredicateFilter
  ( PredicateFilter(..)
  , ShowablePredicate(predMatch)
  , matchFilt
  ) where

import Base1T hiding ( toList )

-- base --------------------------------

import Control.Monad.Fail ( MonadFail )
import Data.Foldable      ( all, and, any )
import Data.List          ( intercalate, zip )
import GHC.Exts           ( toList )

-- natural -----------------------------

import Natural ( length )

-- optparse-plus -----------------------

import OptParsePlus ( OptReader(readM) )

-- parsers -----------------------------

import Text.Parser.Char        ( CharParsing, char, string )
import Text.Parser.Combinators ( sepByNonEmpty, try )

-- parser-plus -------------------------

import ParserPlus ( brackets, whitespaces )

-- regex-with-pcre ---------------------

import Text.RE.PCRE.Text ()

-- textual-plus ------------------------

import TextualPlus ( TextualPlus(textual') )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.TrifectaPlus qualified as TrifectaPlus

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

--------------------

instance (TextualPlus α, Typeable α) ⇒ OptReader (PredicateFilter α) where
  readM = TrifectaPlus.readM

----------------------------------------

parseFilts ∷ ∀ α μ . (MonadFail μ, CharParsing μ, TextualPlus α) ⇒
             μ (NonEmpty (PredicateFilter α))
parseFilts =
  let whitespaced p = whitespaces ⋫ p ⋪ whitespaces
      separator     = whitespaced $ char ','
  in  brackets (whitespaced (textual' `sepByNonEmpty` try (separator)))

----------------------------------------

instance TextualPlus α ⇒ TextualPlus (PredicateFilter α) where
  textual' = (string "⋀" ⋪ whitespaces) ⋫ (EF_Conj ⊳ (whitespaces ⋫ parseFilts))
           ∤ (string "⋁" ⋪ whitespaces) ⋫ (EF_Disj ⊳ (whitespaces ⋫ parseFilts))
           ∤ EF_Pred ⊳ textual'

----------------------------------------

matchFilt ∷ ShowablePredicate α β ⇒ PredicateFilter α → β → 𝔹
matchFilt (EF_Pred p)  e = predMatch p e
matchFilt (EF_Conj ps) e = all (\ p -> matchFilt p e) ps
matchFilt (EF_Disj ps) e = any (\ p -> matchFilt p e) ps

-- that's all, folks! ----------------------------------------------------------
