{-# LANGUAGE UnicodeSyntax #-}
module Brian.EntryFilter
  ( EntryFilter
  , entryMatches
  ) where

import Base1T hiding ( toList )

-- base --------------------------------

import Data.Foldable ( and, or )
import GHC.Exts      ( toList )

-- options-applicative -----------------

import Options.Applicative ( help, long, option, short )

-- optparse-plus -----------------------

import OptParsePlus ( readM )

-- pcre --------------------------------

import PCRE      ( PCRE, (?=~) )
import PCRE.Base ( reSource )

-- regex -------------------------------

import Text.RE.TDFA ( matched )

-- regex-with-pcre ---------------------

import Text.RE.PCRE.Text ()

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.Entry     ( Entry, actresses, title )
import Brian.OptParser ( OptParser(optParse) )

--------------------------------------------------------------------------------

data EntryFilter = EntryFilter { _titleREs :: [PCRE]
                               , _actrsREs :: [PCRE]
                               }

titleREs ∷ Lens' EntryFilter [PCRE]
titleREs = lens _titleREs (\ f t → f { _titleREs = t })

actrsREs ∷ Lens' EntryFilter [PCRE]
actrsREs = lens _actrsREs (\ f t → f { _actrsREs = t })

--------------------

instance Show EntryFilter where
  show (EntryFilter ts as) =
    [fmt|EntryFilter %L %L|] (reSource ⊳ ts) (reSource ⊳ as)

--------------------

instance OptParser EntryFilter where
  optParse = EntryFilter ⊳ many (option readM (ю [ long "title"
                                                 , short 't'
                                                 , help "title match PCRE"
                                                 ]))
                         ⊵ many (option readM (ю [ long "actress"
                                                 , short 'a'
                                                 , help "actress match PCRE"
                                                 ]))

----------------------------------------

entryMatches ∷ EntryFilter → Entry → 𝔹
entryMatches flt e =
  and $ ю [ [ matched $ toText (e ⊣ title) ?=~ pcre | pcre ← flt ⊣ titleREs ]
          , [ or ((matched ∘ (?=~ pcre) ∘ toText) ⊳ (toList $ e ⊣ actresses)) | pcre ← flt ⊣ actrsREs ]
          ]
-- that's all, folks! ----------------------------------------------------------
