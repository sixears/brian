{-# LANGUAGE UnicodeSyntax #-}
module Brian.EntryFilter
  ( EntryFilter
  , entryMatches
  , gFilt
  , titleSTs
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

import Options.Applicative ( eitherReader, help, long, option, short,
                             strOption )

-- optparse-plus -----------------------

import OptParsePlus ( OptReader(readM) )

-- parsers -----------------------------

import Text.Parser.Char        ( char, digit )
import Text.Parser.Combinators ( sepBy )

-- pcre --------------------------------

import PCRE      ( PCRE, (?=~) )
import PCRE.Base ( pcre, reSource )

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

import Brian.Entry     ( Entry, actresses, description, episode, title )
import Brian.Episode   ( EpisodeID(unEpisodeID), epID, epName )
import Brian.OptParser ( OptParser(optParse) )

--------------------------------------------------------------------------------

newtype EpIDFilter = EpIDFilter { unEpIDFilter :: [ℕ] }
  deriving (Eq)

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
                               , _titleSTs :: [𝕋]
                               , _actrsREs :: [PCRE]
                               , _descsREs :: [PCRE]
                               , _epnamREs :: [PCRE]
                               , _epidFs   :: [EpIDFilter]
                               }

titleREs ∷ Lens' EntryFilter [PCRE]
titleREs = lens _titleREs (\ f t → f { _titleREs = t })

titleSTs ∷ Lens' EntryFilter [𝕋]
titleSTs = lens _titleSTs (\ f t → f { _titleSTs = t })

actrsREs ∷ Lens' EntryFilter [PCRE]
actrsREs = lens _actrsREs (\ f t → f { _actrsREs = t })

descsREs ∷ Lens' EntryFilter [PCRE]
descsREs = lens _descsREs (\ f t → f { _descsREs = t })

epnamREs ∷ Lens' EntryFilter [PCRE]
epnamREs = lens _epnamREs (\ f t → f { _epnamREs = t })

epidFs ∷ Lens' EntryFilter [EpIDFilter]
epidFs = lens _epidFs (\ f t → f { _epidFs = t })

--------------------

instance Show EntryFilter where
  show (EntryFilter ts tx as ds ens eis) =
    [fmt|EntryFilter %t|]
      (T.intercalate " " $
       (\ (n,x) → [fmt|{%t: %L}|] n x) ⊳ [ ("titleREs", reSource ⊳ ts)
                                         , ("titleSTs", T.unpack ⊳ tx)
                                         , ("actrsREs", reSource ⊳ as)
                                         , ("descnREs", reSource ⊳ ds)
                                         , ("epnamREs", reSource ⊳ ens)
                                         , ("epidFs"  , toString ⊳ eis)
                                         ])

--------------------

instance OptParser EntryFilter where
  optParse = EntryFilter ⊳ many (option readM (ю [ long "title"
                                                 , short 't'
                                                 , help "title match PCRE"
                                                 ]))
                         ⊵ many (strOption (ю [ long "title-filter"
                                                 , short 'T'
                                                 , help "title LIKE filter"
                                                 ]))
                         ⊵ many (option readM (ю [ long "actress"
                                                 , short 'a'
                                                 , help "actress match PCRE"
                                                 ]))
                         ⊵ many (option readM (ю [ long "description"
                                                 , short 'd'
                                                 , help "description match PCRE"
                                                 ]))
                         ⊵ many (option readM (ю [ long "epname"
                                                 , long "episode-name"
                                                 , short 'e'
                                                 , help "episode name PCRE"
                                                 ]))
                         ⊵ many (option readM (ю [ long "epid"
                                                 , long "episode-id"
                                                 , short 'p'
                                                 , help "episode ID"
                                                 ]))

----------------------------------------

gFilt ∷ Entry → 𝔹
gFilt e =
-- XXX
  let flt = [pcre|(?<!\\bno)\\s+gag|]
  in  or [ matched $ toText(e ⊣ description) ?=~ flt ]

entryMatches ∷ EntryFilter → Entry → 𝔹
entryMatches flt e =
  let actrs_match re  = (matched ∘ (?=~re) ∘ toText) ⊳ toList (e ⊣ actresses)
      epname          = toText ⊳ (e ⊣ episode ≫ view epName)
      epnam_match re  = maybe 𝕱 (\ n → matched $ n ?=~ re) epname
      epid_match epif = fromMaybe 𝕱 (matchEpID epif ⊳ (view epID ⊳ e⊣ episode))
      descn           = toText $ e ⊣ description
  in  and $ ю [ [ matched $ toText(e ⊣ title) ?=~ re | re ← flt ⊣ titleREs ]
              , [ matched $ descn             ?=~ re | re ← flt ⊣ descsREs ]
              , (epnam_match ⊳ flt ⊣ epnamREs)
              , [ or (actrs_match re)                | re ← flt ⊣ actrsREs ]
                -- this should be an OR, i.e., ep ≡ 7.3 or ep ≡ 10.2 ...
              , [ (flt ⊣ epidFs) ≡ [] ∨ or (epid_match ⊳ flt ⊣ epidFs) ]
              ]

-- that's all, folks! ----------------------------------------------------------
