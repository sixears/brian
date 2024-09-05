{-# LANGUAGE UnicodeSyntax #-}
module Brian.EntryData
  ( TagsTable
  , getTagsTable
  , insertEntry
  , readEntry
  ) where

import Base1T  hiding ( toList )
import Prelude ( undefined )

-- base --------------------------------

import Data.List.NonEmpty qualified as NonEmpty

import Data.Foldable      ( Foldable )
import Data.List          ( drop, filter, maximum, zip )
import Data.List.NonEmpty ( nonEmpty )
import Data.Proxy         ( Proxy(Proxy) )
import GHC.Exts           ( IsList(toList), IsString(fromString) )

-- containers --------------------------

import Data.Map.Strict qualified as Map
import Data.Set        qualified as Set

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Indexed ( itoList )

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

import Database.SQLite.Simple         ( Connection, FromRow, NamedParam((:=)),
                                        Only(Only), Query(Query),
                                        SQLData(SQLText), ToRow(toRow),
                                        executeNamed, fromOnly, queryNamed,
                                        query_ )
import Database.SQLite.Simple.ToField ( ToField(toField) )

-- text --------------------------------

import Data.Text qualified as Text

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.BTag        ( BTag(unBTag), TagsRow, btags, tagsRows, unBTags )
import Brian.Entry       ( Entry(Entry), entryTable, tags, title )
import Brian.ID          ( ID(ID, unID) )
import Brian.SQLite      ( ColumnName(unColumnName), TableName, columnID,
                           execute_, fold, insertRow, query )
import Brian.SQLiteError ( AsSQLiteError, throwSQLMiscError )

--------------------------------------------------------------------------------

type TagsTable = Map.Map BTag ID

----------------------------------------

bTags ∷ TagsTable → Set.Set BTag
bTags = fromList ∘ Map.keys

------------------------------------------------------------

{- | A map from column to SQL data -}
newtype SQLDataMap = SQLDataMap { unSQLDataMap :: Map.Map ColumnName SQLData }
  deriving (Show)

instance IsList SQLDataMap where
  type instance Item SQLDataMap = (ColumnName, SQLData)
  fromList = SQLDataMap ∘ fromList
  toList = Map.toList ∘ unSQLDataMap

------------------------------------------------------------

data ColumnTips = NoAttrs | NoInsert deriving (Eq)
data ColumnDesc = ColumnDesc ColumnTips ColumnName

cName ∷ ColumnDesc → 𝕋
cName (ColumnDesc _ n) = unColumnName n

{- Which columns to use for insert -}
insertColumns ∷ [ColumnDesc] → [𝕋]
insertColumns cols =
  let notNoInsert (ColumnDesc tips _) = tips ≢ NoInsert
  in  cName ⊳ filter notNoInsert cols

class Table α where
  type RowType α
  tName   ∷ Proxy α → TableName
  columns ∷ Proxy α → NonEmpty ColumnDesc

data Tags = Tags ()

insertTableRows ∷ ∀ ε α β ω μ .
                  (MonadIO μ, Table α, ToRow (RowType α), FromRow β,
                   AsSQLiteError ε, Printable ε, MonadError ε μ,
                   Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ) ⇒
                  Severity → Proxy α → Connection → [RowType α] → 𝕋 → DoMock
                → μ [(RowType α, [β])]
insertTableRows sev p conn rows extra mck = do
--  execute_ Debug conn "BEGIN TRANSACTION" mck
  let sql = Query $ [fmt|INSERT INTO %T (%L) VALUES (%L)%t%T|] (tName p)
                    (insertColumns ∘ toList $ columns p) (const ("?"∷𝕋) ⊳ (insertColumns ∘ toList $ columns p))
                    (if extra ≡ "" then "" else " ") extra
  results ← forM rows $ \ row → (row,) ⊳ query sev conn sql row ф mck

--  execute_ Debug conn "COMMIT TRANSACTION" mck
  -- execute_ conn $ "ROLLBACK TRANSACTION" -- in emergency…
--  return $ ю results
  return results
------------------------------------------------------------

instance Table Tags where
  type instance RowType Tags = TagsRow
  tName   _ = "Tag"
  columns _ = (ColumnDesc NoInsert "id") :| [ (ColumnDesc NoAttrs "tag") ]

------------------------------------------------------------

{- An insert description for a single row; the table, the data to insert
   (potentially multiple rows; certainly ≥ 1); and any additional clauses
   required. -}
data Insert = Insert { _iTable      :: TableName
                     , _iSQLDataMap :: NonEmpty SQLDataMap
                     , _iExtra      :: 𝕄 𝕋
                     }
  deriving (Show)

iTable ∷ Lens' Insert TableName
iTable = lens _iTable (\ i t → i { _iTable = t })

iSQLDataMap ∷ Lens' Insert (NonEmpty SQLDataMap)
iSQLDataMap = lens _iSQLDataMap (\ i d → i { _iSQLDataMap = d })

iExtra ∷ Lens' Insert (𝕄 𝕋)
iExtra = lens _iExtra (\ i x → i { _iExtra = x })

iEClause ∷ Insert → 𝕋
iEClause i = maybe "" (" "⊕) (i ⊣ iExtra)

iKeys ∷ Insert → [ColumnName]
iKeys = Map.keys ∘ unSQLDataMap ∘ NonEmpty.head ∘ view iSQLDataMap

iQuery ∷ Insert → Query
iQuery i = fromString $
  let keys  = iKeys i
      extra = iEClause i
  in  [fmt|INSERT INTO %T (%L) VALUES (%L)%t%T|] (i ⊣ iTable) keys
                                               (columnID ⊳ keys) (if extra ≡ "" then "" else " ") extra
iData ∷ Insert → [[NamedParam]]
iData =
  fmap (\ (k,v) → columnID k := v) ∘ itoList ∘ unSQLDataMap
                                   ⩺ toList ∘ view iSQLDataMap

------------------------------------------------------------

insertSimple ∷ Connection → Insert → IO ()
insertSimple conn i = forM_ (iData i) $ executeNamed conn (iQuery i)

----------------------------------------

{-
insertSimple' ∷ (MonadIO μ, FromRow r) ⇒ Connection → Insert → μ [[r]]
insertSimple' conn i = liftIO $ forM (iData i) $ queryNamed conn (iQuery i)
-}

----------------------------------------

infix 5 ~
(~) ∷ ToField τ ⇒ ColumnName → τ → (ColumnName,SQLData)
a ~ b = (a, toField b)

--------------------

{-
entryData ∷ Entry → Map.Map ColumnName SQLData
entryData e =  [ "id"          ~ e ⊣ recordNumber
               , "title"       ~ e ⊣ title
               , "medium"      ~ e ⊣ medium
               , "actresses"   ~ toField (e ⊣ actresses)
               , "description" ~ toField (e ⊣ description)
               , "tags"        ~ (""∷𝕋)
               ]
-}

----------------------------------------

tagsInsert ∷ TagsTable → Entry → ([Insert], TagsTable)
tagsInsert tgs e =
  let tgs_max = maximum $ ID 0 : Map.elems tgs
      tg_new = Set.difference (fromList ∘ unBTags $ e ⊣ tags) (bTags tgs)
      tg_insert ∷ [(BTag,ID)]
      tg_insert = zip (toList tg_new) (drop 1 [tgs_max..])

      mk_tag_row (b,i) = ["id" ~ i, "tag" ~ b]

      tg_inserts = case nonEmpty tg_insert of
        𝕹    → []
        𝕵 ys →
          let entry_data = (mk_tag_row ⊳ ys)
          in  [ Insert "Tag" (SQLDataMap ⊳ entry_data) 𝕹 ]
  in  (tg_inserts, Map.union tgs (fromList tg_insert))

----------------------------------------

{-
entryInsert ∷ Entry → Insert
entryInsert e =
  Insert "Record" (pure $ SQLDataMap $ entryData e)
                   (𝕵 "ON CONFLICT (id) DO NOTHING RETURNING (id)")
-}

----------------------------------------

insertTags ∷ (MonadIO μ, AsSQLiteError ε, Printable ε, MonadError ε μ,
              Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ) ⇒
             Connection → TagsTable → Entry → ID → DoMock → μ [(TagsRow,[Only ID])] -- TagsTable
insertTags conn tgs e rid mck = -- liftIO $ do
--  let (ins, tgs') = tagsInsert tgs e
--  forM_ ins $ insertSimple conn
{-
  case nonEmpty (unBTags $ e ⊣ tags) of
    𝕹 → return ()
    𝕵 tg_ids' → do
      let mkref t = ["recordid" ~ rid, "tagid" ~ Map.lookup t tgs']
      insertSimple conn $ Insert "TagRef" (mkref ⊳ tg_ids') 𝕹
  return tgs'
-}

  insertTableRows Informational (Proxy ∷ Proxy Tags) conn (tagsRows $ e ⊣ tags) ("ON CONFLICT (id) DO NOTHING ON CONFLICT (tag) DO NOTHING RETURNING (id)") mck



----------------------------------------

insertEntry ∷ ∀ ε ω μ .
              (MonadIO μ, Default ω, MonadLog (Log ω) μ,
               AsSQLiteError ε, Printable ε, MonadError ε μ,
               MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
              Connection → TagsTable → Entry → DoMock → μ TagsTable
insertEntry conn tgs e mck = do
  execute_ Debug conn "BEGIN TRANSACTION" mck
  let name  = e ⊣ title
  row_id ← insertRow Informational conn entryTable
                     (𝕵 "ON CONFLICT (id) DO NOTHING RETURNING (id)") e
                     [Only (ID 0)] mck
  tgs' ← case row_id of
           [Only (n ∷ ID)] → do
             infoT $ [fmt|inserted %d (%T)|] (unID n) name
             insertTags conn tgs e n mck
--           _ → infoT ([fmt|no insert of %T|] name) ⪼ return tgs

  execute_ Debug conn "COMMIT TRANSACTION" mck
  -- execute_ conn $ "ROLLBACK TRANSACTION" -- in emergency…
  return tgs

----------------------------------------

getTagsTable ∷ MonadIO μ ⇒ Connection → μ TagsTable
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
