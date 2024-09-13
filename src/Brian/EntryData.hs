{-# LANGUAGE UnicodeSyntax #-}
module Brian.EntryData
  ( -- TagsTable
    --   , getTagsTable
    EntryTable
  , insertEntry
  , readEntry
  ) where

import Base1T hiding ( toList )

-- base --------------------------------

import Data.List  ( filter )
import Data.Proxy ( Proxy(Proxy) )
import GHC.Exts   ( IsList(toList) )

-- containers --------------------------

import Data.Map.Strict qualified as Map

-- logging-effect ----------------------

import Control.Monad.Log ( MonadLog, Severity(Debug, Informational) )

-- logs-plus ---------------------------

import Log ( Log, infoT )

-- mockio-log --------------------------

import MockIO.IOClass ( HasIOClass )
import MockIO.Log     ( DoMock, HasDoMock )

-- natural -----------------------------

import Natural ( length )

-- sqlite-simple -----------------------

import Database.SQLite.Simple ( Connection, FromRow, Only(Only), Query(Query),
                                SQLData, ToRow, fromOnly, query_ )
-- text --------------------------------

import Data.Text qualified as Text

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.BTag        ( BTag, TagsRow, TagsTable, btags, tagsRows )
import Brian.Entry       ( Entry(Entry), EntryRow, entryRow, tags, title )
import Brian.ID          ( ID(ID, unID) )
import Brian.SQLite      ( ColumnDesc(ColumnDesc),
                           ColumnFlag(NoInsert, PrimaryKey),
                           ColumnName(unColumnName),
                           ColumnType(CTypeInteger, CTypeText),
                           Table(columns, tName, type RowType), TableName,
                           execute, execute_, insertTableRows, insertTableRows_,
                           query, withinTransaction )
import Brian.SQLiteError ( AsSQLiteError, throwSQLMiscError )

--------------------------------------------------------------------------------

-- type TagsTable = Map.Map BTag ID

------------------------------------------------------------

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
  columns _ = (ColumnDesc "id" CTypeInteger [PrimaryKey]) :| [ ColumnDesc "title" CTypeText []
                                           , ColumnDesc "medium" CTypeText []
                                           , ColumnDesc "actresses" CTypeText []
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

insertTags_ ∷ (MonadIO μ, AsSQLiteError ε, Printable ε, MonadError ε μ,
               Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ) ⇒
              Connection → Entry → DoMock → μ [(TagsRow,[Only ID])]
insertTags_ conn e mck =
  let extra = Text.intercalate " " [ "ON CONFLICT (id) DO NOTHING"
                                   , "ON CONFLICT (tag) DO NOTHING"
                                   , "RETURNING (id)"
                                   ]
      pTags = Proxy ∷ Proxy TagsTable
  in  insertTableRows_ Informational pTags conn (tagsRows $ e ⊣ tags) extra mck

------------------------------------------------------------

insertTags ∷ (MonadIO μ, AsSQLiteError ε, Printable ε, MonadError ε μ,
              Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ) ⇒
             Connection → Entry → DoMock → μ [(TagsRow,[Only ID])]
insertTags conn e mck = withinTransaction conn mck $ insertTags_ conn e mck

----------------------------------------

insertEntry_ ∷ ∀ ε ω μ .
              (MonadIO μ, Default ω, MonadLog (Log ω) μ,
               AsSQLiteError ε, Printable ε, MonadError ε μ,
               MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
              Connection → Entry → DoMock → μ (𝕄 ID)
insertEntry_ conn e mck = do
  let name  = e ⊣ title
  row_ids ← insertTableRows_ Informational (Proxy ∷ Proxy EntryTable) conn [entryRow e] "ON CONFLICT (id) DO NOTHING RETURNING (id)" mck

  case row_ids of
    [(_, [Only (n ∷ ID)])] → do
      infoT $ [fmt|inserted %d (%T)|] (unID n) name
-- XXX pass tags, not e, here
      insertTags_ conn e mck
      let tgs = e ⊣ tags
      let sql = Query $ [fmt|INSERT INTO TagRef (recordid, tagid) SELECT %d,id FROM Tag WHERE tag IN (%L)|] (unID n) (const ("?"∷𝕋) ⊳ toList tgs)
      execute @_ @[𝕋] Informational conn sql (toText ⊳ toList tgs) mck
      return $ 𝕵 n
    _ → return 𝕹

  -- INSERT INTO TagRef (recordid, tagid) SELECT 1,id FROM Tag WHERE tag IN ('country_us','gagtype_cleave');
-- Insert TagRef

  -- execute_ conn $ "ROLLBACK TRANSACTION" -- in emergency…

insertEntry conn e mck = withinTransaction conn mck $ insertEntry_ conn e mck

----------------------------------------

getTagsTable ∷ MonadIO μ ⇒ Connection → μ (Map.Map BTag ID)
getTagsTable conn = liftIO $ do
  let sql = "SELECT tag,id FROM Tag"
  rows ← query_ conn sql
  return $ Map.fromList rows

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
  -- XXX tags

-- that's all, folks! ----------------------------------------------------------
