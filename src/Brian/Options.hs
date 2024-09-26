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

import Options.Applicative ( CommandFields, Mod, Parser, argument, command,
                             info, metavar, progDesc, subparser )

-- textual-plus ------------------------

import TextualPlus.Error.TextualParseError ( AsTextualParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.EntryFilter ( EntryFilter )
import Brian.OptParser   ( optParse )
import Brian.SQLiteError ( AsSQLiteError )

--------------------------------------------------------------------------------

data Mode = ModeCreate (𝕄 File)
          | ModeReCreate (𝕄 File)
          | ModeQuery EntryFilter

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
  let mode_commands ∷ [Mod CommandFields Mode] =
        [ command "create"
                  (info (ModeCreate ⊳ optional (argument readM $ metavar "INPUT-FILE")) (progDesc "build a new database"))
        , command "recreate"
                  (info (ModeReCreate ⊳ optional (argument readM $ metavar "INPUT-FILE")) (progDesc "rebuild a database"))
        , command "query"
                  (info (ModeQuery ⊳ optParse) (progDesc "query the database"))
        ]
  in  Options ⊳ subparser (ю mode_commands)
              ⊵ argument readM (metavar "SQLITE-DB")
--              ⊵ optional (argument readM $ metavar "INPUT-FILE")

-- that's all, folks! ----------------------------------------------------------
