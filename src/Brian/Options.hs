{-# LANGUAGE UnicodeSyntax #-}

module Brian.Options
  ( EntryFilter
  , Mode(..)
  , Options
  , dbFile
    --  , inputFile
  , mode
  , optionsParser
  ) where

import Base1T

-- base --------------------------------

import Control.Applicative ( optional )

-- fpath -------------------------------

import FPath.File      ( File )
import FPath.Parseable ( readM )

-- optparse-applicative ----------------

import Options.Applicative ( CommandFields, Mod, Parser, argument, auto,
                             command, help, info, long, metavar, option,
                             progDesc, short, subparser )

-- optparse-plus -----------------------

import OptParsePlus qualified

-- textual-plus ------------------------

import TextualPlus.Error.TextualParseError ( AsTextualParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.Day         ( Day )
import Brian.EntryFilter ( EntryFilter )
import Brian.OptParser   ( optParse )
import Brian.SQLiteError ( AsSQLiteError )

--------------------------------------------------------------------------------

data Mode = ModeCreate (𝕄 File) (𝕄 Day)
          | ModeReCreate (𝕄 File) (𝕄 Day)
          | ModeAdd (𝕄 File) (𝕄 Day)
          | ModeQuery EntryFilter (𝕄 ℤ)

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
  let input_file = argument readM $ metavar "INPUT-FILE"
      entry_date = option OptParsePlus.readM (ю [ long "entry-date"
                                                , short 'd',help "entry-date" ])
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
        , command "query"
                  (info (ModeQuery ⊳ optParse ⊵ optional (option auto (short 'y' ⊕ long "days" ⊕ help "look back n days' entries"))) (progDesc "query the database"))
        ]
  in  Options ⊳ subparser (ю mode_commands)
              ⊵ argument readM (metavar "SQLITE-DB")

-- that's all, folks! ----------------------------------------------------------
