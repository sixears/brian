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
                             short, value )

-- optparse-plus -----------------------

import OptParsePlus ( readM )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.DBEntryPreFilter ( DBEntryPreFilter, null )
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
             in  QueryOpts ⊳ optional optParse
                           ⊵ prefilt_m
                           ⊵ query_pars_m
                           ⊵ show_sql
                           ⊵ g_filt

----------------------------------------

entryFilter ∷ Lens' QueryOpts (𝕄 EntryFilter)
entryFilter = lens _entryFilter (\ q f → q { _entryFilter = f })

----------------------------------------

entryPreFilter ∷ Lens' QueryOpts DBEntryPreFilter
entryPreFilter = lens _entryPreFilter (\ q f → q { _entryPreFilter = f })

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
