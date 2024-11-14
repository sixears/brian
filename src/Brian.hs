{-# LANGUAGE UnicodeSyntax #-}
module Brian
  ( main
  ) where

import Debug.Trace ( traceShow )

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

import Brian.DBEntryPreFilter qualified as DBEntryPreFilter
import Brian.EntryFilter      qualified as EntryFilter
import Brian.EntryTests       qualified as EntryTests

import Brian.Actress          ( ActressRefTable, ActressTable )
import Brian.BTag             ( TagRefTable, TagTable )
import Brian.Day              ( Day(Day) )
import Brian.DBEntryPreFilter ( DBEntryPreFilter,
                                DBEntryPreFilterItem(DBEntryEntryDateFilter),
                                whereClause )
import Brian.Entry            ( EntryTable, insertEntry, parseEntries,
                                readEntry )
import Brian.EntryFilter      ( EntryFilter, gFilt, matchFilt )
import Brian.ID               ( ID(ID) )
import Brian.Options          ( Mode(ModeAdd, ModeCreate, ModeQuery, ModeReCreate),
                                Options, dbFile, mode, optionsParser )
import Brian.PredicateFilter  ( PredicateFilter(EF_Conj, EF_None, EF_Pred),
                                conj )
import Brian.SQLite           ( Table, createTable, query, reCreateTable,
                                sjoin )
import Brian.SQLiteError      ( AsSQLiteError, UsageSQLiteFPIOTPError,
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
                 Connection → 𝕄 EntryFilter → DoMock → (Only ℤ) → μ ()
maybeDumpEntry c q mck (Only eid) = do
  e ← readEntry c (ID $ fromIntegral eid) mck
  case e of
    𝕵 ē | gFilt ē ∧ maybe 𝕿 (flip matchFilt ē) q → say $ [fmtT|%T\n\n----|] ē
        | otherwise         → return ()
    𝕹   → throwSQLMiscError $ [fmtT|no entry found for %d|] eid

----------------------------------------

queryEntries ∷ ∀ ε ω μ .
               (MonadIO μ, Printable ε, AsSQLiteError ε, MonadError ε μ,
                HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
               Connection → 𝕄 EntryFilter → DBEntryPreFilter → 𝕄 ℕ → DoMock
             → μ ()
queryEntries c q b d mck = traceShow ("b",b) $ do
  let sel = sjoin [ "SELECT DISTINCT Entry.id"
                  , "  FROM Entry, ActressRef, Actress"
                  , "  LEFT JOIN TagRef ON TagRef.recordid = Entry.id"
                  , "  LEFT JOIN Tag    ON TagRef.tagid    = Tag.id"
                  , " WHERE     ActressRef.recordid  = Entry.id"
                  , "       AND ActressRef.actressid = Actress.id"
                  ]
  let ḋ = DBEntryEntryDateFilter ⊳ d
  let ḃ = case (b,ḋ) of
             (ɓ, 𝕵 đ) → 𝕵 $ EF_Conj (ɓ:| [EF_Pred đ])
--             (𝕹  , 𝕵 đ) → 𝕵 $ EF_Pred đ
             _        → 𝕵 b
  (like_clauses,ts) ← maybe (return ([], [])) (first pure ⩺ whereClause) ḃ
  (like_clauses,ts) ← do (lc,ts_) ← whereClause DBEntryPreFilter.gFilt
                         return (like_clauses ⊕ [lc], ts ⊕ ts_)
  (like_clauses,ts) ← whereClause $ conj b $ conj DBEntryPreFilter.gFilt (fromMaybe EF_None $ EF_Pred ∘ DBEntryEntryDateFilter ⊳ d)
  eids ← let sql   = Query $
--               if like_clauses ≡ []
--               then sel
               {- else -} [fmt|%t AND %t|] sel like_clauses -- (T.intercalate " AND " like_clauses)
         in  query Informational c sql ts [] mck
  forM_ eids (maybeDumpEntry c q mck)

----------------------------------------

readBrian ∷ ∀ ε ω μ .
            (MonadIO μ, MonadLog (Log ω) μ, Default ω,HasIOClass ω,HasDoMock ω,
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
          ModeQuery    q b d → queryEntries c q b d mck
          ModeCreate   f d   → build c d NoReCreateTables f mck
          ModeReCreate f d   → build c d ReCreateTables   f mck
          ModeAdd      f d   → build c d NoCreateTables   f mck

----------------------------------------

main ∷ IO ()
main =
  let desc ∷ 𝕋 = "manipulate a brianDB"
  in  getArgs ≫ stdMain desc optionsParser (doMain @UsageSQLiteFPIOTPError)


-- tests -----------------------------------------------------------------------

{-| unit tests -}
tests ∷ TestTree
tests =
  testGroup "Brian" [ EntryTests.tests, EntryFilter.tests
                    , DBEntryPreFilter.tests ]

_test ∷ IO ExitCode
_test = runTestTree tests

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
