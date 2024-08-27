{-# LANGUAGE UnicodeSyntax #-}
module Brian
  ( main
  ) where

import Base1T

-- base --------------------------------

import Control.Applicative ( optional )
import Control.Monad       ( foldM_, (=<<) )
import System.Environment  ( getArgs )

-- fpath -------------------------------

import FPath.File      ( File(FileR) )
import FPath.Parseable ( readM )
import FPath.RelFile   ( relfile )

-- HTTP --------------------------------

import Network.HTTP ( getResponseBody, postRequestWithBody, simpleHTTP )

-- logging-effect ----------------------

import Control.Monad.Log ( LoggingT, MonadLog, Severity(Informational) )

-- logs-plus ---------------------------

import Log ( Log )

-- mockio-log --------------------------

import MockIO.IOClass ( HasIOClass )
import MockIO.Log     ( DoMock(DoMock, NoMock), HasDoMock, MockIOClass )

-- monadio-plus ------------------------

import MonadIO.OpenFile ( readFileUTF8Lenient )

-- optparse-applicative ----------------

import Options.Applicative ( CommandFields, Mod, Parser, argument, command,
                             info, metavar, progDesc, subparser )

-- sqlite-simple -----------------------

import Database.SQLite.Simple ( Connection, open )

-- stdmain --------------------------------

import StdMain            ( stdMain )
import StdMain.UsageError ( AsUsageError, throwUsageT )

-- tagsoup -----------------------------

import Text.HTML.TagSoup ( Tag, parseTags )

-- text --------------------------------

import Data.Text    ( pack )
import Data.Text.IO qualified as TextIO

-- textual-plus ------------------------

import TextualPlus.Error.TextualParseError ( AsTextualParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.Entry       ( entryTable, parseEntries, printEntry )
import Brian.EntryData   ( getTagsTable, insertEntry )
import Brian.SQLite      ( Column(Column), ColumnFlag(FlagUnique, PrimaryKey),
                           ColumnType(CTypeInteger, CTypeText), Table(Table),
                           TableFlag(ForeignKey, OkayIfExists), createTable,
                           fold, reCreateTable )
import Brian.SQLiteError ( AsSQLiteError, UsageSQLiteFPIOTPError )

--------------------------------------------------------------------------------

openURL' ∷ String → String → IO String
openURL' x t = let content_type = "application/x-www-form-urlencoded"
                   postRequest  = postRequestWithBody x content_type t
               in  getResponseBody =<< simpleHTTP postRequest

brian ∷ MonadIO μ ⇒ μ String
brian = liftIO $ openURL' "http://brianspage.com/query.php" "description=gag"

data ReCreateTables = ReCreateTables | NoReCreateTables

buildTables ∷ ∀ ε ω μ .
              (MonadIO μ,
               AsSQLiteError ε,AsTextualParseError ε,Printable ε,MonadError ε μ,
               MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
              Connection → ReCreateTables → DoMock → μ ()
buildTables conn recreate mck = do
  let create = case recreate of
                 ReCreateTables   → reCreateTable
                 NoReCreateTables → createTable
  create conn entryTable mck
  create conn (Table "Tag" [ OkayIfExists ]
         [ Column "id"          CTypeInteger [PrimaryKey]
         , Column "tag"         CTypeText    [FlagUnique]
         ]) mck
  create conn (Table "TagRef" [ OkayIfExists, ForeignKey ["recordid"] ]
         [ Column "recordid"    CTypeInteger ф
         , Column "tagid"       CTypeInteger ф
         ]) mck

data Mode = ModeCreate | ModeReCreate | ModeQuery

data Options ε = Options { _mode      :: Mode
                         , _dbFile    :: File
                         , _inputFile :: 𝕄 File
                           --                         , _createTables :: 𝕄 (SQLLog () ε)
                         }

mode ∷ Lens' (Options ε) Mode
mode = lens _mode (\ o m → o { _mode = m })

dbFile ∷ Lens' (Options ε) File
dbFile = lens _dbFile (\ o f → o { _dbFile = f })

inputFile ∷ Lens' (Options ε) (𝕄 File)
inputFile = lens _inputFile (\ o f → o { _inputFile = f })

optionsParser ∷ (AsSQLiteError ε, AsTextualParseError ε, Printable ε) ⇒
                Parser (Options ε)
optionsParser =
  let mode_commands ∷ [Mod CommandFields Mode] =
        [ command "create"
                  (info (pure ModeCreate) (progDesc "build a new database"))
        , command "recreate"
                  (info (pure ModeReCreate) (progDesc "rebuild a database"))
        , command "query"
                  (info (pure ModeQuery) (progDesc "query the database"))
        ]
  in  Options ⊳ subparser (ю mode_commands)
              ⊵ argument readM (metavar "SQLITE-DB")
              ⊵ optional (argument readM $ metavar "INPUT-FILE")

----------------------------------------

doMain ∷ (AsIOError ε, AsTextualParseError ε, AsUsageError ε, AsSQLiteError ε,
          Printable ε) ⇒
         DoMock → (Options ε) → LoggingT (Log MockIOClass) (ExceptT ε IO) ()
doMain mck opts = do
  case mck of
    DoMock → throwUsageT "dry-run not yet implemented"
    NoMock → return ()

  conn ← case opts ⊣ dbFile of
           FileR r | r ≡ [relfile|-|] → return 𝕹
           x                          → liftIO $ 𝕵 ⊳ open (toString x)
  t    ← case opts ⊣ inputFile of
           𝕵 f → readFileUTF8Lenient f
           𝕹   → pack ⊳ brian

  let ts ∷ [Tag 𝕋] = parseTags t

  case conn of
    𝕹   → parseEntries ts ≫ mapM_ printEntry
    𝕵 c → do
      case opts ⊣ mode of
        ModeCreate   → buildTables c NoReCreateTables mck
        ModeReCreate → buildTables c ReCreateTables   mck
        ModeQuery    → fold @_ @_ @(ℤ,𝕋) @_ Informational c "SELECT id,title FROM Entry" () () (\ () (eid,title) → TextIO.putStrLn $ [fmt|%d - %t|] eid title ) () mck
      tags_table ← getTagsTable c
      parseEntries ts ≫ foldM_ (\ tgs e → insertEntry c tgs e mck) tags_table

----------------------------------------

main ∷ IO ()
main =
  let desc ∷ 𝕋 = "manipulate a brianDB"
  in  getArgs ≫ stdMain desc optionsParser (doMain @UsageSQLiteFPIOTPError)

-- that's all, folks! ----------------------------------------------------------
