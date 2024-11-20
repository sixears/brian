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

import Control.Applicative ( optional )

-- optparse-applicative ----------------

import Options.Applicative ( Parser, auto, flag, help, long, metavar, option,
                             short, str, value )

-- optparse-plus -----------------------

import OptParsePlus ( readM )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.DBEntryPreFilter ( DBEntryPreFilter, actressFilter, conj,
                                descFilter, null, titleFilter )
import Brian.EntryFilter      ( EntryFilter )
import Brian.OptParser        ( OptParser(optParse) )
import Brian.ShowSQL          ( ShowSQL(NoShowSQL, ShowSQL) )

--------------------------------------------------------------------------------

data GFilt = GFilt | NoGFilt deriving (Eq)

------------------------------------------------------------

data QueryOpts = QueryOpts { _entryFilter    :: 𝕄 EntryFilter
                           , _entryPreFilter :: DBEntryPreFilter
                           , _ageDays        :: 𝕄 ℕ
                           , _showSQL        :: ShowSQL
                           , _gfilt          :: GFilt
                           , _titlePreFilt   :: 𝕄 𝕋
                           , _actressPreFilt :: 𝕄 𝕋
                           , _descPreFilt    :: 𝕄 𝕋
                           }

--------------------

instance OptParser QueryOpts where
  optParse = let prefilt_h  = help "entry DB pre-filter"
                 prefilt_m  ∷ Parser DBEntryPreFilter
                 prefilt_m  =
                   option readM $ ю [ short 'b', prefilt_h, value null
                                    , metavar "PREDICATE" ]
                 query_pars =
                   let hlp = "look back n days' entries"
                   in  option auto (ю [ short 'y', long "days", help hlp])
                 query_pars_m = optional query_pars
                 show_sql = flag NoShowSQL ShowSQL (ю [ long "show-sql"
                                                      , help "show sql"])
                 g_filt   = flag GFilt     NoGFilt (ю [ long "no-g-filt"
                                                      , help "no g filter"])
                 help_T   =
                   help "prefilt title match (inserts % unless % present)"
                 filt_T   = optional $ option str $
                   ю [ short 'T', help_T, metavar "TITLE" ]
                 help_A   =
                   help "prefilt actress match (inserts % unless % present)"
                 filt_A   = optional $ option str $
                   ю [ short 'A', help_A, metavar "ACTRESS" ]
                 help_D   =
                   help "prefilt description match (inserts % unless % present)"
                 filt_D   = optional $ option str $
                   ю [ short 'D', help_D, metavar "DESCRIPTION" ]
             in  QueryOpts ⊳ optional optParse
                           ⊵ prefilt_m
                           ⊵ query_pars_m
                           ⊵ show_sql
                           ⊵ g_filt
                           ⊵ filt_T
                           ⊵ filt_A
                           ⊵ filt_D

----------------------------------------

entryFilter ∷ Lens' QueryOpts (𝕄 EntryFilter)
entryFilter = lens _entryFilter (\ q f → q { _entryFilter = f })

----------------------------------------

entryPreFilter ∷ QueryOpts → DBEntryPreFilter
entryPreFilter q =
  foldr conj (_entryPreFilter q) [ maybe null titleFilter   (_titlePreFilt q)
                                 , maybe null actressFilter (_actressPreFilt q)
                                 , maybe null descFilter    (_descPreFilt q)
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
