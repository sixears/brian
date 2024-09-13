{-# LANGUAGE UnicodeSyntax #-}
module Brian.EntryData
  ( EntryTable
  , insertEntry
  , readEntry
  ) where

import Base1T hiding ( toList )

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

import Database.SQLite.Simple ( Connection, Only(Only), Query(Query), SQLData,
                                fromOnly )
-- text --------------------------------

import Data.Text qualified as Text

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.BTag        ( BTags, TagsRow, TagsTable, btags, tagsRows )
import Brian.Entry       ( Entry(Entry), EntryRow, entryRow, tags, title )
import Brian.ID          ( ID(unID) )
import Brian.SQLite      ( ColumnDesc(ColumnDesc), ColumnFlag(PrimaryKey),
                           ColumnName, ColumnType(CTypeInteger, CTypeText),
                           Table(columns, tName, type RowType), TableName,
                           execute, insertTableRows_, query, withinTransaction )
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
               , ColumnDesc "description" CTypeText [] ]

------------------------------------------------------------

{- An insert description for a single row; the table, the data to insert
   (potentially multiple rows; certainly ≥ 1); and any additional clauses
   required. -}
data Insert = Insert { _iTable      :: TableName
                     , _iSQLDataMap :: NonEmpty SQLDataMap
                     , _iExtra      :: 𝕄 𝕋
                     }
  deriving (Show)

------------------------------------------------------------

insertTagRefs_ ∷ (MonadIO μ,
                  AsSQLiteError ε, Printable ε, MonadError ε μ,
                  HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
                 Connection → ID → BTags → DoMock → μ ()
insertTagRefs_ conn n tgs =
  let sql =
        let insert = "INSERT INTO TagRef (recordid, tagid)"
        in  Query $ [fmt|%t SELECT %d,id FROM Tag WHERE tag IN (%L)|]
                    insert (unID n) (const ("?"∷𝕋) ⊳ toList tgs)
  in  execute @_ @[𝕋] Informational conn sql (toText ⊳ toList tgs)

----------------------------------------

insertTags_ ∷ (MonadIO μ, AsSQLiteError ε, Printable ε, MonadError ε μ,
               Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ) ⇒
              Connection → BTags → DoMock → μ [(TagsRow,[Only ID])]
insertTags_ conn tgs mck =
  let extra = Text.intercalate " " [ "ON CONFLICT (id) DO NOTHING"
                                   , "ON CONFLICT (tag) DO NOTHING"
                                   , "RETURNING (id)"
                                   ]
      pTags = Proxy ∷ Proxy TagsTable
  in  insertTableRows_ Informational pTags conn (tagsRows tgs) extra mck

----------------------------------------

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
      let tgs = e ⊣ tags
      _ ← insertTags_ conn tgs mck
      insertTagRefs_ conn n tgs mck
      return $ 𝕵 n
    _ → return 𝕹


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
  let sql = "SELECT title,medium,actresses,description FROM Entry WHERE ID = ?"
  query Informational conn sql (Only eid) [] mck ≫ \ case
    []                    → return 𝕹

    [(ttle,mdm,act,desc)] → do
      let sql' = "SELECT tag FROM Tag,TagRef WHERE recordid = ? AND id = tagid"
      tgs ← btags ⊳ (fromOnly ⊳⊳query Informational conn sql' (Only eid) [] mck)
      return ∘ 𝕵 $ Entry eid ttle (𝕵 mdm) act tgs desc

    xs                    →
      throwSQLMiscError $ [fmtT|too many (%d) entries found for %d|]
                          (unID eid) (length xs)

-- that's all, folks! ----------------------------------------------------------
