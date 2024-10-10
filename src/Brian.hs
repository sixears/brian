{-# LANGUAGE UnicodeSyntax #-}
module Brian
  ( main
  ) where

import Base1T

-- base --------------------------------

import Control.Monad      ( (=<<) )
import Data.Function      ( flip )
import Data.Maybe         ( catMaybes, fromMaybe )
import Data.Proxy         ( Proxy(Proxy) )
import System.Environment ( getArgs )

-- fpath -------------------------------

import FPath.File ( File )

-- HTTP --------------------------------

import Network.HTTP ( getResponseBody, postRequest, postRequestWithBody,
                      simpleHTTP )

-- logging-effect ----------------------

import Control.Monad.Log ( LoggingT, MonadLog, Severity(Debug, Informational) )

-- logs-plus ---------------------------

import Log ( Log )

-- natural -----------------------------

import Natural ( length )

-- mockio-log --------------------------

import MockIO.IOClass ( HasIOClass )
import MockIO.Log     ( DoMock(DoMock, NoMock), HasDoMock, MockIOClass, logio,
                        noticeIO', warnIO' )

-- monadio-plus ------------------------

import MonadIO          ( say )
import MonadIO.OpenFile ( readFileUTF8Lenient )

-- safe-exceptions ---------------------

import Control.Exception.Safe ( finally )

-- sqlite-simple -----------------------

import Database.SQLite.Simple ( Connection, Only(Only), Query(Query), close,
                                open )

-- stdmain --------------------------------

import StdMain            ( stdMain )
import StdMain.UsageError ( AsUsageError, throwUsageT )

-- tagsoup -----------------------------

import Text.HTML.TagSoup ( Tag, parseTags )

-- text --------------------------------

import Data.Text qualified as T

-- textual-plus ------------------------

import TextualPlus.Error.TextualParseError ( AsTextualParseError )

-- time --------------------------------

import Data.Time.Clock ( getCurrentTime, utctDay )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.Actress     ( ActressRefTable, ActressTable )
import Brian.BTag        ( TagRefTable, TagTable )
import Brian.Day         ( Day(Day) )
import Brian.Entry       ( parseEntries )
import Brian.EntryData   ( EntryTable, insertEntry, readEntry )
import Brian.EntryFilter ( entryMatches, gFilt, titleSTs )
import Brian.ID          ( ID(ID) )
import Brian.Options     ( EntryFilter,
                           Mode(ModeAdd, ModeCreate, ModeQuery, ModeReCreate),
                           Options, dbFile, mode, optionsParser )
import Brian.SQLite      ( Table, createTable, query, query_, reCreateTable )
import Brian.SQLiteError ( AsSQLiteError, UsageSQLiteFPIOTPError,
                           throwSQLMiscError )

--------------------------------------------------------------------------------

openURL ∷ String → 𝕄 String → IO String
openURL x t = let content_type = "application/x-www-form-urlencoded"
                  request  = case t of
                    𝕵 t' → postRequestWithBody x content_type t'
                    𝕹    → postRequest x
              in  getResponseBody =<< simpleHTTP request

brian ∷ (MonadIO μ, MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
        μ String
brian = do
  s ← liftIO $ openURL "http://brianspage.com/query.php" (𝕵 "description=gag")
  logio Debug ([fmtT|read %d bytes|] (length s)) NoMock
  if length s < 200
  then logio Debug ([fmtT|read '%s'|] s) NoMock
  else return ()
  return s

------------------------------------------------------------

data CreateTables = ReCreateTables | NoReCreateTables | NoCreateTables

buildTables ∷ ∀ ε ω μ .
              (MonadIO μ,
               AsSQLiteError ε,AsTextualParseError ε,Printable ε,MonadError ε μ,
               MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
              Connection → CreateTables → DoMock → μ ()
buildTables conn recreate mck = do
  let create ∷ Table α ⇒ Connection → Proxy α → DoMock → μ ()
      create = case recreate of
                 ReCreateTables   → reCreateTable
                 NoReCreateTables → createTable
                 NoCreateTables   → \ _conn _proxy _mock → return ()
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
    𝕵 e' | gFilt e' ∧ entryMatches q e' → say $ [fmtT|%T\n\n----|] e'
         | otherwise         → return ()
    𝕹    → throwSQLMiscError $ [fmtT|no entry found for %d|] eid

----------------------------------------

queryEntries ∷ (MonadIO μ, Printable ε, AsSQLiteError ε, MonadError ε μ,
                HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
               Connection → EntryFilter → DoMock → μ ()
queryEntries c q mck = do
  let sel = "SELECT id FROM Entry"
  eids ← case q ⊣ titleSTs of
              []  → query_ Informational c (Query sel) [] mck
              ts  → let likes = T.intercalate " AND title LIKE " (const "?"⊳ ts)
                        sql   = Query $ [fmt|%t WHERE title LIKE %t|] sel likes
                    in  query Informational c sql ts [] mck
  forM_ eids (maybeDumpEntry c q mck)

----------------------------------------

readBrian ∷ (MonadIO μ, MonadLog (Log ω) μ, Default ω,HasIOClass ω,HasDoMock ω,
             AsIOError ε, MonadError ε μ) ⇒ 𝕄 File → μ [Tag 𝕋]
readBrian input = do
  t ← case input of
    𝕵 f → readFileUTF8Lenient f
    𝕹   → T.pack ⊳ brian
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
        let build cnn d recreate f mock = do
              today ← liftIO $ utctDay ⊳ getCurrentTime
              buildTables cnn recreate mock
              let go e = insertEntry c (fromMaybe (Day today) d) e mock
              entries ← readBrian f ≫ parseEntries
              noticeIO' $ [fmt|found %d entries|] (length entries)
              ids ← mapM go entries
              warnIO' $ [fmt|inserted %d entries|] (length $ catMaybes ids)
              return ()
        case opts ⊣ mode of
          ModeQuery    q   → queryEntries c q mck
          ModeCreate   f d → build c d NoReCreateTables f mck
          ModeReCreate f d → build c d ReCreateTables   f mck
          ModeAdd      f d → build c d NoCreateTables   f mck

----------------------------------------

main ∷ IO ()
main =
  let desc ∷ 𝕋 = "manipulate a brianDB"
  in  getArgs ≫ stdMain desc optionsParser (doMain @UsageSQLiteFPIOTPError)

-- that's all, folks! ----------------------------------------------------------
