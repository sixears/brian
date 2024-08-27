{-# LANGUAGE UnicodeSyntax #-}
module Brian.EntryData
  ( getTagsTable
  , insertEntry
  ) where

import Base1T

-- base --------------------------------

import Data.List.NonEmpty qualified as NonEmpty

import Data.List          ( drop, maximum, zip )
import Data.List.NonEmpty ( nonEmpty )
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

-- sqlite-simple -----------------------

import Database.SQLite.Simple         ( Connection, NamedParam((:=)),
                                        Only(Only), Query, SQLData,
                                        executeNamed, query_ )
import Database.SQLite.Simple.ToField ( ToField(toField) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.BTag        ( BTag, unBTags )
import Brian.Entry       ( Entry, entryTable, tags, title )
import Brian.ID          ( ID(ID, unID) )
import Brian.SQLite      ( ColumnName, TableName, columnID, execute_,
                           insertRow )
import Brian.SQLiteError ( AsSQLiteError )

--------------------------------------------------------------------------------

type TagsTable = Map.Map BTag ID

----------------------------------------

bTags ∷ TagsTable → Set.Set BTag
bTags = fromList ∘ Map.keys

------------------------------------------------------------

newtype EntryData = EntryData { unEntryData :: Map.Map ColumnName SQLData }
  deriving (Show)

instance IsList EntryData where
  type instance Item EntryData = (ColumnName, SQLData)
  fromList = EntryData ∘ fromList
  toList = Map.toList ∘ unEntryData

------------------------------------------------------------

data Insert = Insert { _iTable     :: TableName
                     , _iEntryData :: NonEmpty EntryData
                     , _iExtra     :: 𝕄 𝕋
                     }
  deriving (Show)

iTable ∷ Lens' Insert TableName
iTable = lens _iTable (\ i t → i { _iTable = t })

iEntryData ∷ Lens' Insert (NonEmpty EntryData)
iEntryData = lens _iEntryData (\ i d → i { _iEntryData = d })

iExtra ∷ Lens' Insert (𝕄 𝕋)
iExtra = lens _iExtra (\ i x → i { _iExtra = x })

iEClause ∷ Insert → 𝕋
iEClause i = maybe "" (" "⊕) (i ⊣ iExtra)

iKeys ∷ Insert → [ColumnName]
iKeys = Map.keys ∘ unEntryData ∘ NonEmpty.head ∘ view iEntryData

iQuery ∷ Insert → Query
iQuery i = fromString $
  let keys = iKeys i
  in  [fmt|INSERT INTO %T (%L) VALUES (%L)%T|] (i ⊣ iTable) keys
                                               (columnID ⊳ keys) (iEClause i)
iData ∷ Insert → [[NamedParam]]
iData =
  fmap (\ (k,v) → (columnID k := v)) ∘ itoList ∘ unEntryData
                                     ⩺ Base1T.toList ∘ view iEntryData

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
      tg_insert = zip (Base1T.toList tg_new) (drop 1 [tgs_max..])

      mk_tag_row (b,i) = ["id" ~ i, "tag" ~ b]

      tg_inserts = case nonEmpty tg_insert of
        𝕹    → []
        𝕵 ys →
          let entry_data = (mk_tag_row ⊳ ys)
          in  [ Insert "Tag" (EntryData ⊳ entry_data) 𝕹 ]
  in  (tg_inserts, Map.union tgs (fromList tg_insert))

----------------------------------------

{-
entryInsert ∷ Entry → Insert
entryInsert e =
  Insert "Record" (pure $ EntryData $ entryData e)
                   (𝕵 "ON CONFLICT (id) DO NOTHING RETURNING (id)")
-}

----------------------------------------

insertEntry ∷ ∀ ε ω μ .
              (MonadIO μ, Default ω, MonadLog (Log ω) μ,
               AsSQLiteError ε, Printable ε, MonadError ε μ,
               MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
              Connection → TagsTable → Entry → DoMock → μ TagsTable
insertEntry conn tgs e mck = do
  execute_ Debug conn "BEGIN TRANSACTION" mck
  let name  = e ⊣ title
  row_id ← insertRow @_ @_ @(Only ID) Informational conn entryTable
                     (𝕵 "ON CONFLICT (id) DO NOTHING RETURNING (id)") e
                     [Only (ID 0)] mck
  tgs' ← case row_id of
           [Only (n ∷ ID)] → do
             infoT $ [fmt|inserted %d (%T)|] (unID n) name
             insertTags conn tgs e n

           _ → infoT ([fmt|no insert of %T|] name) ⪼ return tgs

  execute_ Debug conn "COMMIT TRANSACTION" mck
  -- execute_ conn $ "ROLLBACK TRANSACTION" -- in emergency…
  return tgs'

----------------------------------------

insertTags ∷ MonadIO μ ⇒ Connection → TagsTable → Entry → ID → μ TagsTable
insertTags conn tgs e rid = liftIO $ do
  let (ins, tgs') = tagsInsert tgs e
  forM_ ins $ insertSimple conn
  case nonEmpty (unBTags $ e ⊣ tags) of
    𝕹 → return ()
    𝕵 tg_ids' → do
      let mkref t = ["recordid" ~ rid, "tagid" ~ Map.lookup t tgs']
      insertSimple conn $ Insert "TagRef" (mkref ⊳ tg_ids') 𝕹
  return tgs'

----------------------------------------

getTagsTable ∷ MonadIO μ ⇒ Connection → μ TagsTable
getTagsTable conn = liftIO $ do
  let sql = "SELECT tag,id FROM Tag"
  rows ← query_ conn sql
  return $ Map.fromList rows

-- that's all, folks! ----------------------------------------------------------
