{-# LANGUAGE UnicodeSyntax #-}

module Brian.Options
  ( Mode(..)
  , Options
  , dbFile
  , mode
  , optionsParser
  ) where

import Base1T

-- base --------------------------------

import Control.Applicative ( optional )

-- fpath -------------------------------

import FPath.File      ( File )
import FPath.Parseable qualified

-- optparse-applicative ----------------

import Options.Applicative ( CommandFields, Mod, Parser, argument, command,
                             help, info, long, metavar, option, progDesc, short,
                             subparser )

-- optparse-plus -----------------------

import OptParsePlus ( readM )

-- textual-plus ------------------------

import TextualPlus.Error.TextualParseError ( AsTextualParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.Day         ( Day )
import Brian.OptParser   ( OptParser(optParse) )
import Brian.QueryOpts   ( QueryOpts )
import Brian.SQLiteError ( AsSQLiteError )

--------------------------------------------------------------------------------

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
