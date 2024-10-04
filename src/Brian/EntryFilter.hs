{-# LANGUAGE UnicodeSyntax #-}
module Brian.EntryFilter
  ( EntryFilter
  , entryMatches
  ) where

import Base1T hiding ( toList )

-- base --------------------------------

import Data.Foldable ( and, or )
import Data.List     ( repeat, zip )
import Data.Maybe    ( fromMaybe )
import GHC.Exts      ( toList )
import Text.Read     ( read )

-- lens --------------------------------

import Control.Lens.Getter ( view )

-- options-applicative -----------------

import Options.Applicative ( eitherReader, help, long, option, short )

-- optparse-plus -----------------------

import OptParsePlus ( OptReader(readM) )

-- parsers -----------------------------

import Text.Parser.Char        ( char, digit )
import Text.Parser.Combinators ( sepBy )

-- pcre --------------------------------

import PCRE      ( PCRE, (?=~) )
import PCRE.Base ( reSource )

-- regex -------------------------------

import Text.RE.TDFA ( matched )

-- regex-with-pcre ---------------------

import Text.RE.PCRE.Text ()

-- text --------------------------------

import Data.Text qualified as T

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus ( TextualPlus(textual'), parseTextual )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.Entry     ( Entry, actresses, episode, title )
import Brian.Episode   ( EpisodeID(unEpisodeID), epID, epName )
import Brian.OptParser ( OptParser(optParse) )

--------------------------------------------------------------------------------

newtype EpIDFilter = EpIDFilter { unEpIDFilter :: [ℕ] }

instance Printable EpIDFilter where
  print = P.text ∘ T.intercalate "." ∘ fmap (T.pack ∘ show) ∘ unEpIDFilter

instance TextualPlus EpIDFilter where
  textual' = EpIDFilter ⊳ (read ⊳ some digit) `sepBy` char '.'

instance OptReader EpIDFilter where
  readM = eitherReader $ parseTextual

matchEpID ∷ EpIDFilter → EpisodeID → 𝔹
matchEpID (EpIDFilter fs) (unEpisodeID → ds) =
   and [ maybe 𝕱 (≡ f) d  | (f,d) ← zip fs ((𝕵 ⊳ ds) ⊕ repeat 𝕹) ]

------------------------------------------------------------

data EntryFilter = EntryFilter { _titleREs :: [PCRE]
                               , _actrsREs :: [PCRE]
                               , _epnamREs :: [PCRE]
                               , _epidFs   :: [EpIDFilter]
                               }

titleREs ∷ Lens' EntryFilter [PCRE]
titleREs = lens _titleREs (\ f t → f { _titleREs = t })

actrsREs ∷ Lens' EntryFilter [PCRE]
actrsREs = lens _actrsREs (\ f t → f { _actrsREs = t })

epnamREs ∷ Lens' EntryFilter [PCRE]
epnamREs = lens _epnamREs (\ f t → f { _epnamREs = t })

epidFs ∷ Lens' EntryFilter [EpIDFilter]
epidFs = lens _epidFs (\ f t → f { _epidFs = t })

--------------------

instance Show EntryFilter where
  show (EntryFilter ts as ens eis) =
    [fmt|EntryFilter {titleREs: %L} {actrsREs: %L} {epnamREs: %L} {epidFs: %L}|]
      (reSource ⊳ ts) (reSource ⊳ as) (reSource ⊳ ens) (toText ⊳ eis)

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
                         ⊵ many (option readM (ю [ long "epid"
                                                 , long "episode-id"
                                                 , short 'E'
                                                 , help "episode ID"
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
-- this should be an OR, i.e., ep ≡ 7.3 or ep ≡ 10.2 ...
              , [or [ fromMaybe 𝕱 (matchEpID epif ⊳ (view epID ⊳ e ⊣ episode))        | epif ← flt ⊣ epidFs ]]
              ]

-- that's all, folks! ----------------------------------------------------------
