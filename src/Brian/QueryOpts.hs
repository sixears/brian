{-# LANGUAGE UnicodeSyntax #-}
module Brian.QueryOpts
  ( GFilt(..)
  , QueryOpts
  , ageDays
  , entryFilter
  , entryPreFilter
  , gfilt
  , showSQL
  ) where

import Base1T

-- base --------------------------------

import Data.String qualified as S

import Control.Applicative ( optional )

-- optparse-applicative ----------------

import Options.Applicative ( Parser, auto, flag, help, long, metavar, option,
                             short, str, value )

-- optparse-plus -----------------------

import OptParsePlus ( OptReader(readM) )

-- pcre --------------------------------

import PCRE ( PCRE )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.DBEntryPreFilter qualified as DBEntryPreFilter
import Brian.EntryFilter      qualified as EntryFilter

import Brian.DBEntryPreFilter ( DBEntryPreFilter, actressFilter, descFilter,
                                epidFilter )
import Brian.EntryFilter      ( EntryFilter )
import Brian.OptParser        ( OptParser(optParse), optArg )
import Brian.ShowSQL          ( ShowSQL(NoShowSQL, ShowSQL) )

--------------------------------------------------------------------------------

data GFilt = GFilt | NoGFilt deriving (Eq)

------------------------------------------------------------

data QueryOpts = QueryOpts { _entryFilter    :: EntryFilter
                           , _entryPreFilter :: DBEntryPreFilter
                           , _ageDays        :: 𝕄 ℕ
                           , _showSQL        :: ShowSQL
                           , _gfilt          :: GFilt
                           , _titlePreFilt   :: 𝕄 𝕋
                           , _actressPreFilt :: 𝕄 𝕋
                           , _descPreFilt    :: 𝕄 𝕋
                           , _epidPreFilt    :: 𝕄 𝕋
                           , _titleFilter    :: 𝕄 PCRE
                           , _actressFilter  :: 𝕄 PCRE
                           , _descFilter     :: 𝕄 PCRE
                           }

--------------------

instance OptParser QueryOpts where
  optParse = let prefilt_m  ∷ Parser DBEntryPreFilter
                 prefilt_m  =
                   option readM $ ю [ short 'b', value DBEntryPreFilter.null ]
                                  ⊕ DBEntryPreFilter.textualHelpMods
                 query_pars =
                   let hlp = "only show entries from later than n days ago"
                   in  option auto (ю [ short 'y', long "days", help hlp])
                 query_pars_m = optional query_pars
                 show_sql = flag NoShowSQL ShowSQL (ю [ long "show-sql"
                                                      , help "show sql"])
                 g_filt   = flag GFilt     NoGFilt (ю [ long "no-g-filt"
                                                      , help "no g filter"])
                 help_t   =
                   help "prefilt title match (inserts % unless % present)"
                 filt_t   = optional $ option str $
                   ю [ short 't', help_t, metavar "TITLE" ]
                 help_T   =
                   help "title match PCRE"
                 filt_T   = optional $ option readM $
                   ю [ short 'T', help_T, metavar "TITLE" ]
                 help_a   =
                   help "prefilt actress match (inserts % unless % present)"
                 filt_a   = optional $ option str $
                   ю [ short 'a', help_a, metavar "ACTRESS" ]
                 help_A   =
                   help "actress match PCRE"
                 filt_A   = optional $ option readM $
                   ю [ short 'A', help_A, metavar "ACTRESS" ]
                 help_d   =
                   help "prefilt description match (inserts % unless % present)"
                 filt_d   = optional $ option str $
                   ю [ short 'd', help_d, metavar "DESCRIPTION" ]
                 help_D   =
                   help "prefilt description match (inserts % unless % present)"
                 filt_D   = optional $ option readM $
                   ю [ short 'D', help_D, metavar "DESCRIPTION" ]
                 help_p   =
                   help $ S.unwords [ "prefilt episodeid (note text match;"
                                    , "postfixed % unless % present, format"
                                    , "e.g., 1.2.3 (no leading '0's))"
                                    ]
                 filt_p   = optional $ option str $
                   ю [ short 'p', help_p, metavar "EPISODE ID" ]
             in  QueryOpts ⊳ optArg
                           ⊵ prefilt_m
                           ⊵ query_pars_m
                           ⊵ show_sql
                           ⊵ g_filt
                           ⊵ filt_t
                           ⊵ filt_a
                           ⊵ filt_d
                           ⊵ filt_p
                           ⊵ filt_T
                           ⊵ filt_A
                           ⊵ filt_D

----------------------------------------

entryFilter ∷ QueryOpts → EntryFilter
entryFilter q =
  foldr EntryFilter.conj (_entryFilter q)
        [ maybe EntryFilter.null EntryFilter.titleFilter (_titleFilter q)
        , maybe EntryFilter.null EntryFilter.actressFilter (_actressFilter q)
        , maybe EntryFilter.null EntryFilter.descFilter (_descFilter q)
        ]

----------------------------------------

entryPreFilter ∷ QueryOpts → DBEntryPreFilter
entryPreFilter q =
  foldr DBEntryPreFilter.conj (_entryPreFilter q)
        [ maybe DBEntryPreFilter.null DBEntryPreFilter.titleFilter (_titlePreFilt q)
        , maybe DBEntryPreFilter.null actressFilter (_actressPreFilt q)
        , maybe DBEntryPreFilter.null descFilter    (_descPreFilt q)
        , maybe DBEntryPreFilter.null epidFilter    (_epidPreFilt q)
        ]

----------------------------------------

ageDays ∷ Lens' QueryOpts (𝕄 ℕ)
ageDays = lens _ageDays (\ q f → q { _ageDays = f })

----------------------------------------

showSQL ∷ Lens' QueryOpts ShowSQL
showSQL = lens _showSQL (\ q f → q { _showSQL = f })

----------------------------------------

gfilt ∷ Lens' QueryOpts GFilt
gfilt = lens _gfilt (\ q f → q { _gfilt = f })

-- that's all, folks! ----------------------------------------------------------
