{-# LANGUAGE UnicodeSyntax #-}
module Brian
  ( main
  ) where

import Base1T
-- import Prelude ( undefined )

-- base --------------------------------

import Control.Monad      ( (=<<) )
import Data.Function      ( flip )
import Data.Proxy         ( Proxy(Proxy) )
import System.Environment ( getArgs )

-- fpath -------------------------------

import FPath.File ( File )

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
import Brian.Entry       ( parseEntries )
import Brian.EntryData   ( EntryTable, insertEntry, readEntry )
import Brian.EntryFilter ( entryMatches )
import Brian.ID          ( ID(ID) )
import Brian.Options     ( EntryFilter,
                           Mode(ModeCreate, ModeQuery, ModeReCreate), Options,
                           dbFile, mode, optionsParser )
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

maybeDumpEntry ∷ ∀ ε ω μ .
                 (MonadIO μ, Default ω, MonadLog (Log ω) μ,
                  AsSQLiteError ε, Printable ε, MonadError ε μ,
                  MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
                 Connection → EntryFilter → DoMock → (Only ℤ) → μ ()
maybeDumpEntry c q mck (Only eid) = do
  e ← readEntry c (ID $ fromIntegral eid) mck
  case e of
    𝕵 e' | entryMatches q e' → say $ [fmtT|%T\n\n----|] e'
         | otherwise         → return ()
    𝕹    → throwSQLMiscError $ [fmtT|no entry found for %d|] eid

----------------------------------------

queryEntries ∷ (MonadIO μ, Printable ε, AsSQLiteError ε, MonadError ε μ,
                HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
               Connection → EntryFilter → DoMock → μ ()
queryEntries c q mck = do
  let sql = "SELECT id FROM Entry"
  eids ← query_ Informational c sql [] mck
  forM_ eids (maybeDumpEntry c q mck)

----------------------------------------

readBrian ∷ (MonadIO μ, AsIOError ε, MonadError ε μ) ⇒ 𝕄 File → μ [Tag 𝕋]
readBrian input = do
  t ← case input of
    𝕵 f → readFileUTF8Lenient f
    𝕹   → pack ⊳ brian
  return $ parseTags t

----------------------------------------

doMain ∷ (AsIOError ε, AsTextualParseError ε, AsUsageError ε, AsSQLiteError ε,
          Printable ε) ⇒
         DoMock → (Options ε) → LoggingT (Log MockIOClass) (ExceptT ε IO) ()
doMain mck opts = do
  case mck of
    DoMock → throwUsageT "dry-run not yet implemented"
    NoMock → return ()

  do
      c ← liftIO $ open (toString $ opts ⊣ dbFile)
      flip finally (liftIO $ close c) $ do
        let build cnn recreate f mock = do
              buildTables cnn recreate mock
              let go e = insertEntry c e mock
              readBrian f ≫ parseEntries ≫ mapM_ go
        case opts ⊣ mode of
          ModeQuery    q → queryEntries c q mck
          ModeCreate   f → build c NoReCreateTables f mck
          ModeReCreate f → build c ReCreateTables   f mck

----------------------------------------

main ∷ IO ()
main =
  let desc ∷ 𝕋 = "manipulate a brianDB"
  in  getArgs ≫ stdMain desc optionsParser (doMain @UsageSQLiteFPIOTPError)

-- that's all, folks! ----------------------------------------------------------
