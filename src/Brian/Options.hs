{-# LANGUAGE UnicodeSyntax #-}

module Brian.Options
  ( GFilt(..)
  , Mode(..)
  , Options
  , QueryOpts
  , ShowSQL(..)
  , ageDays
  , dbFile
  , entryFilter
  , entryPreFilter
  , gfilt
  , mode
  , optionsParser
  , showSQL
  ) where

import Base1T

-- base --------------------------------

import Control.Applicative ( optional )

-- fpath -------------------------------

import FPath.File      ( File )
import FPath.Parseable qualified

-- optparse-applicative ----------------

import Options.Applicative ( CommandFields, Mod, Parser, argument, auto,
                             command, flag, help, info, long, metavar, option,
                             progDesc, short, subparser, value )

-- optparse-plus -----------------------

import OptParsePlus ( readM )

-- textual-plus ------------------------

import TextualPlus.Error.TextualParseError ( AsTextualParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.Day              ( Day )
import Brian.DBEntryPreFilter ( DBEntryPreFilter, null )
import Brian.EntryFilter      ( EntryFilter )
import Brian.OptParser        ( OptParser(optParse) )
import Brian.SQLiteError      ( AsSQLiteError )

--------------------------------------------------------------------------------

data ShowSQL = ShowSQL | NoShowSQL deriving (Eq)
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

------------------------------------------------------------

data Mode = ModeCreate (𝕄 File) (𝕄 Day)
          | ModeReCreate (𝕄 File) (𝕄 Day)
          | ModeAdd (𝕄 File) (𝕄 Day)
          | ModeQuery QueryOpts

------------------------------------------------------------

data Options ε = Options { _mode   :: Mode
                         , _dbFile :: File
                         }

--------------------

mode ∷ Lens' (Options ε) Mode
mode = lens _mode (\ o m → o { _mode = m })

--------------------

dbFile ∷ Lens' (Options ε) File
dbFile = lens _dbFile (\ o f → o { _dbFile = f })

----------------------------------------

optionsParser ∷ (AsSQLiteError ε, AsTextualParseError ε, Printable ε) ⇒
                Parser (Options ε)
optionsParser =
  let input_file = argument FPath.Parseable.readM $ metavar "INPUT-FILE"
      entry_date = option OptParsePlus.readM (ю [ long "entry-date"
                                                , short 'd',help "entry-date" ])
      query_desc = progDesc "query the database"
      query_info = {- let prefilt_h  = help "entry DB pre-filter"
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
                   in  -} info (ModeQuery ⊳ {- optional optParse
                                       ⊵ prefilt_m ⊵ query_pars_m ⊵ show_sql -} optParse)
                            query_desc
      mode_commands ∷ [Mod CommandFields Mode] =
        [ command "create"
                  (info (ModeCreate ⊳ optional input_file
                                    ⊵ optional entry_date)
                        (progDesc "build a new database"))
        , command "recreate"
                  (info (ModeReCreate ⊳ optional input_file
                                      ⊵ optional entry_date)
                        (progDesc "rebuild a database"))
        , command "add"
                  (info (ModeAdd ⊳ optional input_file
                                 ⊵ optional entry_date)
                        (progDesc "add to an existing database"))
        , command "query" query_info

        ]
  in  Options ⊳ subparser (ю mode_commands)
              ⊵ argument FPath.Parseable.readM (metavar "SQLITE-DB")

instance (AsSQLiteError ε, AsTextualParseError ε, Printable ε) ⇒ OptParser (Options ε) where
  optParse = optionsParser

-- that's all, folks! ----------------------------------------------------------
