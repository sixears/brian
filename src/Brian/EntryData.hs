{-# LANGUAGE UnicodeSyntax #-}
module Brian.EntryData
  ( EntryTable
  , insertEntry
  , readEntry
  ) where

import Base1T      hiding ( toList )
import Debug.Trace ( traceShow )

-- base --------------------------------

import Data.Proxy ( Proxy(Proxy) )
import GHC.Exts   ( IsList(toList) )

-- containers --------------------------

import Data.Map.Strict qualified as Map

-- logging-effect ----------------------

import Control.Monad.Log ( MonadLog, Severity(Informational) )

-- logs-plus ---------------------------

import Log ( Log, infoT )

-- mockio-log --------------------------

import MockIO.IOClass ( HasIOClass )
import MockIO.Log     ( DoMock, HasDoMock )

-- natural -----------------------------

import Natural ( length )

-- sqlite-simple -----------------------

import Database.SQLite.Simple ( Connection, Only(Only), Query(Query), SQLData )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.Actress     ( insertEntryActresses_, readActresses )
import Brian.BTag        ( insertEntryTags_, readTags )
import Brian.Entry       ( Entry(Entry), EntryRow, actresses, entryRow, tags,
                           title )
import Brian.Episode     ( Episode(Episode), EpisodeName, epi )
import Brian.ID          ( ID(unID) )
import Brian.SQLite      ( ColumnDesc(ColumnDesc), ColumnFlag(PrimaryKey),
                           ColumnName, ColumnType(CTypeInteger, CTypeText),
                           Table(columns, tName, type RowType),
                           insertTableRows_, query, withinTransaction )
import Brian.SQLiteError ( AsSQLiteError, throwSQLMiscError )

--------------------------------------------------------------------------------

{- | A map from column to SQL data -}
newtype SQLDataMap = SQLDataMap { unSQLDataMap :: Map.Map ColumnName SQLData }
  deriving (Show)

instance IsList SQLDataMap where
  type instance Item SQLDataMap = (ColumnName, SQLData)
  fromList = SQLDataMap ∘ fromList
  toList = Map.toList ∘ unSQLDataMap

------------------------------------------------------------

data EntryTable

------------------------------------------------------------

instance Table EntryTable where
  type instance RowType EntryTable = EntryRow
  tName   _ = "Entry"
  columns _ =  ( ColumnDesc "id"          CTypeInteger [PrimaryKey] )
            :| [ ColumnDesc "title"       CTypeText []
               , ColumnDesc "medium"      CTypeText []
               , ColumnDesc "actresses"   CTypeText []
               , ColumnDesc "description" CTypeText []
               , ColumnDesc "episodeid"   CTypeText []
               , ColumnDesc "episodename" CTypeText []
               ]

------------------------------------------------------------

insertEntry_ ∷ ∀ ε ω μ .
               (MonadIO μ, Default ω, MonadLog (Log ω) μ,
                AsSQLiteError ε, Printable ε, MonadError ε μ,
                MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
               Connection → Entry → DoMock → μ (𝕄 ID)
insertEntry_ conn e mck = do
  let name  = e ⊣ title
  row_ids ← insertTableRows_ Informational (Proxy ∷ Proxy EntryTable) conn
                             [entryRow e]
                             "ON CONFLICT (id) DO NOTHING RETURNING (id)" mck

  case row_ids of
    [(_, [Only (n ∷ ID)])] → do
      infoT $ [fmt|inserted %d (%T)|] (unID n) name
      insertEntryTags_ conn n (e ⊣ tags) mck
      insertEntryActresses_ conn n (e ⊣ actresses) mck
      return $ 𝕵 n
    _ → return 𝕹

--------------------

insertEntry ∷ ∀ ε ω μ .
              (MonadIO μ, Default ω, MonadLog (Log ω) μ,
               AsSQLiteError ε, Printable ε, MonadError ε μ,
               MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
              Connection → Entry → DoMock → μ (𝕄 ID)
insertEntry conn e mck = withinTransaction conn mck $ insertEntry_ conn e mck

----------------------------------------

readEntry ∷ ∀ ε ω μ .
            (MonadIO μ, Default ω, MonadLog (Log ω) μ,
             AsSQLiteError ε, Printable ε, MonadError ε μ,
             MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
            Connection → ID → DoMock → μ (𝕄 Entry)
readEntry conn eid mck = do
  let fields ∷ [𝕋]
      fields = [ "title", "medium", "description", "episodeid", "episodename" ]
      sql = Query $ [fmt|SELECT %L FROM Entry WHERE ID = ?|] fields
  query Informational conn sql (Only eid) [] mck ≫ \ case
    []                    → return 𝕹

    [(ttle,mdm,desc,epid,epname ∷ 𝕄 EpisodeName)] → traceShow ("epid",epid) $ do
      tgs  ← readTags      conn eid mck
      acts ← readActresses conn eid mck
      return ∘ 𝕵 $ Entry eid ttle (𝕵 mdm) acts tgs desc (epi epid epname)

    xs                    →
      throwSQLMiscError $ [fmtT|too many (%d) entries found for %d|]
                          (unID eid) (length xs)

-- that's all, folks! ----------------------------------------------------------
