{-# LANGUAGE UnicodeSyntax #-}
module Brian
  ( main
  ) where

import Base1T

-- base --------------------------------

import Control.Applicative ( optional )
import Control.Monad       ( (=<<) )
import Data.Function       ( flip )
import Data.Proxy          ( Proxy(Proxy) )
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

import MonadIO          ( say )
import MonadIO.OpenFile ( readFileUTF8Lenient )

-- optparse-applicative ----------------

import Options.Applicative ( CommandFields, Mod, Parser, argument, command,
                             info, metavar, progDesc, subparser )

-- safe-exceptions ---------------------

import Control.Exception.Safe ( finally )

-- sqlite-simple -----------------------

import Database.SQLite.Simple ( Connection, Only(Only), close, open )

-- stdmain --------------------------------

import StdMain            ( stdMain )
import StdMain.UsageError ( AsUsageError, throwUsageT )

-- tagsoup -----------------------------

import Text.HTML.TagSoup ( Tag, parseTags )

-- text --------------------------------

import Data.Text ( pack )

-- textual-plus ------------------------

import TextualPlus.Error.TextualParseError ( AsTextualParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.Actress     ( ActressRefTable, ActressTable )
import Brian.BTag        ( TagRefTable, TagTable )
import Brian.Entry       ( parseEntries, printEntry )
import Brian.EntryData   ( EntryTable, insertEntry, readEntry )
import Brian.ID          ( ID(ID) )
import Brian.SQLite      ( Table, createTable, query_, reCreateTable )
import Brian.SQLiteError ( AsSQLiteError, UsageSQLiteFPIOTPError,
                           throwSQLMiscError )

--------------------------------------------------------------------------------

openURL' ∷ String → String → IO String
openURL' x t = let content_type = "application/x-www-form-urlencoded"
                   postRequest  = postRequestWithBody x content_type t
               in  getResponseBody =<< simpleHTTP postRequest

brian ∷ MonadIO μ ⇒ μ String
brian = liftIO $ openURL' "http://brianspage.com/query.php" "description=gag"

------------------------------------------------------------

data ReCreateTables = ReCreateTables | NoReCreateTables

buildTables ∷ ∀ ε ω μ .
              (MonadIO μ,
               AsSQLiteError ε,AsTextualParseError ε,Printable ε,MonadError ε μ,
               MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
              Connection → ReCreateTables → DoMock → μ ()
buildTables conn recreate mck = do
  let create ∷ Table α ⇒ Connection → Proxy α → DoMock → μ ()
      create = case recreate of
                 ReCreateTables   → reCreateTable
                 NoReCreateTables → createTable
  create conn (Proxy ∷ Proxy EntryTable) mck
  create conn (Proxy ∷ Proxy TagTable) mck
  create conn (Proxy ∷ Proxy TagRefTable) mck
  create conn (Proxy ∷ Proxy ActressTable) mck
  create conn (Proxy ∷ Proxy ActressRefTable) mck

------------------------------------------------------------

data Mode = ModeCreate | ModeReCreate | ModeQuery

------------------------------------------------------------

data Options ε = Options { _mode      :: Mode
                         , _dbFile    :: File
                         , _inputFile :: 𝕄 File
                         }

--------------------

mode ∷ Lens' (Options ε) Mode
mode = lens _mode (\ o m → o { _mode = m })

--------------------

dbFile ∷ Lens' (Options ε) File
dbFile = lens _dbFile (\ o f → o { _dbFile = f })

--------------------

inputFile ∷ Lens' (Options ε) (𝕄 File)
inputFile = lens _inputFile (\ o f → o { _inputFile = f })

----------------------------------------

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

dumpEntry ∷ ∀ ε ω μ .
            (MonadIO μ, Default ω, MonadLog (Log ω) μ,
             AsSQLiteError ε, Printable ε, MonadError ε μ,
             MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
            Connection → DoMock → (Only ℤ) → μ ()
dumpEntry c mck (Only eid) = do
  e ← readEntry c (ID $ fromIntegral eid) mck
  case e of
    𝕵 e' → say $ [fmtT|%T\n\n----|] e'
    𝕹    → throwSQLMiscError $ [fmtT|no entry found for %d|] eid


queryEntries ∷ (MonadIO μ, Printable ε, AsSQLiteError ε, MonadError ε μ,
                HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
               Connection → DoMock → μ ()
queryEntries c mck = do
  let sql = "SELECT id FROM Entry"
  eids ← query_ Informational c sql [] mck
  forM_ eids (dumpEntry c mck)

----------------------------------------

doMain ∷ (AsIOError ε, AsTextualParseError ε, AsUsageError ε, AsSQLiteError ε,
          Printable ε) ⇒
         DoMock → (Options ε) → LoggingT (Log MockIOClass) (ExceptT ε IO) ()
doMain mck opts = do
  case mck of
    DoMock → throwUsageT "dry-run not yet implemented"
    NoMock → return ()

  t    ← case opts ⊣ inputFile of
           𝕵 f → readFileUTF8Lenient f
           𝕹   → pack ⊳ brian

  let ts ∷ [Tag 𝕋] = parseTags t

  case opts ⊣ dbFile of
    FileR r | r ≡ [relfile|-|] → parseEntries ts ≫ mapM_ printEntry
    x                          → do
      c ← liftIO $ open (toString x)
      flip finally (liftIO $ close c) $ do
        let build cnn recreate mock = do
              buildTables cnn recreate mock
              let go e = insertEntry c e mock
              parseEntries ts ≫ mapM_ go
        case opts ⊣ mode of
          ModeCreate   → build c NoReCreateTables mck
          ModeReCreate → build c ReCreateTables   mck
          ModeQuery    → queryEntries c mck

----------------------------------------

main ∷ IO ()
main =
  let desc ∷ 𝕋 = "manipulate a brianDB"
  in  getArgs ≫ stdMain desc optionsParser (doMain @UsageSQLiteFPIOTPError)

-- that's all, folks! ----------------------------------------------------------
