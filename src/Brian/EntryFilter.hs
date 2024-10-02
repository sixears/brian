{-# LANGUAGE UnicodeSyntax #-}
module Brian.EntryFilter
  ( EntryFilter
  , entryMatches
  ) where

import Base1T hiding ( toList )

-- base --------------------------------

import Data.Foldable ( and, or )
import GHC.Exts      ( toList )

-- lens --------------------------------

import Control.Lens.Getter ( view )

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

import Brian.Entry     ( Entry, actresses, episode, title )
import Brian.Episode   ( epName )
import Brian.OptParser ( OptParser(optParse) )

--------------------------------------------------------------------------------

data EntryFilter = EntryFilter { _titleREs :: [PCRE]
                               , _actrsREs :: [PCRE]
                               , _epnamREs :: [PCRE]
                               }

titleREs ∷ Lens' EntryFilter [PCRE]
titleREs = lens _titleREs (\ f t → f { _titleREs = t })

actrsREs ∷ Lens' EntryFilter [PCRE]
actrsREs = lens _actrsREs (\ f t → f { _actrsREs = t })

epnamREs ∷ Lens' EntryFilter [PCRE]
epnamREs = lens _epnamREs (\ f t → f { _epnamREs = t })

--------------------

instance Show EntryFilter where
  show (EntryFilter ts as ens) =
    [fmt|EntryFilter {titleREs: %L} {actrsREs: %L} {epnamREs: %L}|]
      (reSource ⊳ ts) (reSource ⊳ as) (reSource ⊳ ens)

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
                         ⊵ many (option readM (ю [ long "epname"
                                                 , long "episode-name"
                                                 , short 'e'
                                                 , help "episode name PCRE"
                                                 ]))

----------------------------------------

entryMatches ∷ EntryFilter → Entry → 𝔹
entryMatches flt e =
  let actrs_match pcre = (matched ∘ (?=~pcre) ∘ toText) ⊳ toList (e ⊣ actresses)
      epname           = toText ⊳ (e ⊣ episode ≫ view epName)
      epnam_match pcre = maybe 𝕱 (\ n → matched $ n ?=~ pcre) epname
  in  and $ ю [ [ matched $ toText(e ⊣ title) ?=~ pcre | pcre ← flt ⊣ titleREs ]
              , [ epnam_match pcre                     | pcre ← flt ⊣ epnamREs ]
              , [ or (actrs_match pcre)                | pcre ← flt ⊣ actrsREs ]
              ]

-- that's all, folks! ----------------------------------------------------------
