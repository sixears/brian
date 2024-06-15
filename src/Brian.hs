{-# LANGUAGE UnicodeSyntax #-}
module Brian
  ( main
  ) where

import Base1T
import Debug.Trace ( traceShow )
import Prelude     ( undefined )

-- base --------------------------------

import Data.List.NonEmpty qualified as NonEmpty

import Control.Applicative ( optional )
import Control.Monad       ( foldM_, (=<<) )
import Data.Function       ( flip )
import Data.List           ( drop, filter, maximum, zip )
import Data.List.NonEmpty  ( nonEmpty )
import Data.Monoid         ( mconcat )
import GHC.Exts            ( IsList(toList), IsString(fromString) )
import System.Environment  ( getArgs )

-- containers --------------------------

import Data.Map.Strict qualified as Map
import Data.Set        qualified as Set

-- fpath -------------------------------

import FPath.File      ( File(FileR) )
import FPath.Parseable ( readM )
import FPath.RelFile   ( relfile )

-- HTTP --------------------------------

import Network.HTTP ( getResponseBody, postRequestWithBody, simpleHTTP )

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Indexed ( itoList )

-- logs-plus ---------------------------

import Log ( Log, infoT )

-- logging-effect ----------------------

import Control.Monad.Log ( LoggingT, MonadLog, Severity(Debug, Informational) )

-- mockio-log --------------------------

import MockIO.IOClass ( HasIOClass(ioClass), IOClass(IORead, IOWrite) )
import MockIO.Log     ( DoMock(DoMock, NoMock), HasDoMock, MockIOClass,
                        mkIOLME )

-- monadio-plus ------------------------

import MonadIO.OpenFile ( readFileUTF8Lenient )

-- monaderror-io -----------------------

import MonadError.IO ( wrapAsIOErr )

-- mtl ---------------------------------

import Control.Monad.Except ( throwError )

-- neat-interpolation ------------------

import NeatInterpolation ( trimming )

-- optparse-applicative ----------------

import Options.Applicative ( Parser, argument, flag', help, long, metavar,
                             short )

-- safe-exceptions ---------------------

import Control.Exception      qualified as Exception
import Control.Exception.Safe ( try )

-- sqlite-simple -----------------------

import Database.SQLite.Simple qualified as SQLite

import Database.SQLite.Simple         ( Connection, FormatError, FromRow,
                                        NamedParam((:=)), Only(Only), Query,
                                        SQLData, SQLError, executeNamed, open,
                                        queryNamed, query_ )
import Database.SQLite.Simple.ToField ( ToField(toField) )

-- stdmain --------------------------------

import StdMain            ( stdMain )
import StdMain.UsageError ( AsUsageError, UsageFPIOTPError, throwUsageT )

-- tagsoup -----------------------------

import Text.HTML.TagSoup ( Tag, parseTags )

-- text --------------------------------

import Data.Text ( intercalate, pack, unpack )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus.Error.TextualParseError ( AsTextualParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.BTag        ( BTag, unBTags )
import Brian.Entry       ( Entry, actresses, description, medium, parseEntries,
                           printEntry, recordNumber, tags, title )
import Brian.ID          ( ID(ID, unID) )
import Brian.SQLiteError ( AsSQLiteError(_SQLiteError), UsageSQLiteFPIOTPError,
                           toAsSQLiteError, toSQLiteError )

--------------------------------------------------------------------------------

openURL' ∷ String → String → IO String
openURL' x t = let content_type = "application/x-www-form-urlencoded"
                   postRequest  = postRequestWithBody x content_type t
               in  getResponseBody =<< simpleHTTP postRequest

brian ∷ MonadIO μ ⇒ μ String
brian = liftIO $ openURL' "http://brianspage.com/query.php" "description=gag"


data ColumnType = CTypeText | CTypeInteger
instance Printable ColumnType where
  print CTypeText    = P.text "TEXT"
  print CTypeInteger = P.text "INTEGER"

data ColumnFlag = PrimaryKey | FlagUnique

instance Printable ColumnFlag where
  print PrimaryKey = P.text "PRIMARY KEY"
  print FlagUnique = P.text "UNIQUE"

data Column = Column { cname  :: ColumnName
                     , ctype  :: ColumnType
                     , cflags :: [ColumnFlag]
                     }

data TableFlag = OkayIfExists
               | ForeignKey [ColumnName]
  deriving (Eq, Show)

instance Printable Column where
  print (Column { cname, ctype, cflags }) =
    let flags = ю $ (" " ⊕) ∘ toText ⊳ cflags
    in  P.text $ [fmt|%T %T %t|] cname ctype flags

catch ∷ (MonadIO μ, Exception ε, MonadError α μ) ⇒ IO β → (ε → IO α) → μ β
catch io h =
  liftIO ((𝕽 ⊳ io) `Exception.catch` (𝕷 ⩺ h)) ≫ either throwError return

catches ∷ (MonadIO μ, MonadError α μ) ⇒ IO β → [Exception.Handler α] → μ β
catches io h = do
    liftIO ((𝕽 ⊳ io) `Exception.catches` (𝕷 ⊳⊳ h)) ≫ either throwError return

execute_ ∷ ∀ ε ω μ . (MonadIO μ, AsSQLiteError ε, MonadError ε μ,
                      MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω)⇒
           Severity → Connection → Query → DoMock → μ ()
execute_ sev conn sql =
  let handlers = [ Exception.Handler $ return ∘ toAsSQLiteError @SQLError
                 , Exception.Handler $ return ∘ toAsSQLiteError @FormatError
                 ]
--  in  (SQLite.execute_ conn sql) `catches` handlers
  in  mkIOLME sev IOWrite ("FIXME"∷𝕋) () ((SQLite.execute_ conn sql) `catches` handlers)


-- createTable ∷ MonadIO μ ⇒ Connection → μ ()
createTable ∷ ∀ ε ω μ .
              (MonadIO μ, AsSQLiteError ε, MonadError ε μ,
               MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
              Connection → 𝕋 → [TableFlag] → [Column] → DoMock → μ ()
createTable conn tname tflags cols mck =
  let exists = if OkayIfExists ∈ tflags then "IF NOT EXISTS " else ""
      columns = intercalate ", " $ toText ⊳ cols
      sql = fromString $ [fmt|CREATE TABLE %t%t (%t)|] exists tname columns
  in  execute_ Informational conn sql mck

reCreateTable ∷ ∀ ε ω μ .
                (MonadIO μ, AsSQLiteError ε, MonadError ε μ,
                 MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
                Connection → 𝕋 → [TableFlag] → [Column] → DoMock → μ ()
reCreateTable conn tname tflags cols mck = do
  execute_ Informational conn (fromString $ [fmt|DROP TABLE %T|] tname) mck
  createTable conn tname (filter (≢ OkayIfExists) tflags) cols mck
-- create table tagref ( recordid INTEGER, tagid INTEGER ) FOREIGN KEY( recordid ) REFERENCES Records(id), FOREIGN KEY (tagid) REFERENCES Tags(Id)
-- create table tags ( id INTEGER PRIMARY KEY, tag TEXT UNIQUE )
{-
makeTable ∷ MonadIO μ ⇒ Connection → μ ()
makeTable conn = liftIO $ do
  -- CR mpearce: it would be nice if we had a direct qq for Query
  let sql = fromString $ unpack [trimming|
              CREATE TABLE IF NOT EXISTS Records
                ( id          INTEGER PRIMARY KEY
                , title       TEXT
                , medium      TEXT
                , actresses   TEXT
                , tags        TEXT
                , description TEXT)
            |]
  execute_ conn sql
-}

newtype Table = Table { unTable :: 𝕋 }
  deriving newtype (IsString, Show)

instance Printable Table where print = P.text ∘ unTable

newtype ColumnName = ColumnName { unColumnName :: 𝕋 }
  deriving newtype (Eq, IsString, Ord, Show)

instance Printable ColumnName where print = P.text ∘ unColumnName

columnID ∷ ColumnName → 𝕋
columnID = (":"⊕) ∘ unColumnName

infix 5 ~
(~) ∷ ToField τ ⇒ ColumnName → τ → (ColumnName,SQLData)
a ~ b = (a, toField b)

newtype EntryData = EntryData { unEntryData :: Map.Map ColumnName SQLData }
  deriving (Show)

instance IsList EntryData where
  type instance Item EntryData = (ColumnName, SQLData)
  fromList = EntryData ∘ fromList
  toList = Map.toList ∘ unEntryData

data Insert = Insert { _iTable     :: Table
                     , _iEntryData :: NonEmpty EntryData
                     , _iExtra     :: 𝕄 𝕋
                     }
  deriving (Show)

iTable ∷ Lens' Insert Table
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

insertSimple ∷ Connection → Insert → IO ()
insertSimple conn i = forM_ (iData i) $ executeNamed conn (iQuery i)

insertSimple' ∷ (MonadIO μ, FromRow r) ⇒ Connection → Insert → μ [[r]]
insertSimple' conn i = liftIO $ forM (iData i) $ queryNamed conn (iQuery i)

entryData ∷ Entry → Map.Map ColumnName SQLData
entryData e =  [ "id"          ~ e ⊣ recordNumber
               , "title"       ~ e ⊣ title
               , "medium"      ~ e ⊣ medium
               , "actresses"   ~ toField (e ⊣ actresses)
               , "description" ~ toField (e ⊣ description)
               , "tags"        ~ (""∷𝕋)
               ]

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
          in  [ Insert "Tags" (EntryData ⊳ entry_data) 𝕹 ]
  in  (tg_inserts, Map.union tgs (fromList tg_insert))

entryInsert ∷ Entry → Insert
entryInsert e =
  Insert "Records" (pure $ EntryData $ entryData e)
                   (𝕵 "ON CONFLICT (id) DO NOTHING RETURNING (id)")

type TagsTable = Map.Map BTag ID

bTags ∷ TagsTable → Set.Set BTag
bTags = fromList ∘ Map.keys

insertEntry ∷ ∀ ε ω μ .
              (MonadIO μ, Default ω, MonadLog (Log ω) μ,
               AsSQLiteError ε, MonadError ε μ,
               MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
              Connection → TagsTable → Entry → DoMock → μ TagsTable
insertEntry conn tgs e mck = do
  execute_ Debug conn "BEGIN TRANSACTION" mck
  let insert = entryInsert e
      name  = e ⊣ title
  tgs' ← insertSimple' conn insert ≫ \ case
    [[Only (n :: ID)]] → do
      infoT $ [fmt|inserted %d (%T)|] (unID n) name
      insertTags conn tgs e n
    _ → infoT ([fmt|no insert of %T|] name) ⪼ return tgs
  execute_ Debug conn "COMMIT TRANSACTION" mck
  -- execute_ conn $ "ROLLBACK TRANSACTION" -- in emergency…
  return tgs'

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

getTagsTable ∷ MonadIO μ ⇒ Connection → μ TagsTable
getTagsTable conn = liftIO $ do
  let sql = "SELECT tag,id FROM Tags"
  rows ← query_ conn sql
  return $ Map.fromList rows

data ReCreateTables = ReCreateTables | NoReCreateTables

buildTables ∷ ∀ ε ω μ .
              (MonadIO μ, AsSQLiteError ε, AsTextualParseError ε,MonadError ε μ,
               MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
              Connection → ReCreateTables → DoMock → μ ()
buildTables conn recreate mck = do
  let create = case recreate of
                 ReCreateTables   → reCreateTable
                 NoReCreateTables → createTable
  create conn "Records" [ OkayIfExists ]
         [ Column "id"          CTypeInteger [PrimaryKey]
         , Column "title"       CTypeText    ф
         , Column "medium"      CTypeText    ф
         , Column "actresses"   CTypeText    ф
         , Column "tags"        CTypeText    ф
         , Column "description" CTypeText    ф
         ] mck
  create conn "Tags" [ OkayIfExists ]
         [ Column "id"          CTypeInteger [PrimaryKey]
         , Column "tag"         CTypeText    [FlagUnique]
         ] mck
  create conn "TagRef" [ OkayIfExists, ForeignKey ["recordid"] ]
         [ Column "recordid"    CTypeInteger [PrimaryKey]
         , Column "tagid"       CTypeInteger ф
         ] mck
--  tags_table ← getTagsTable conn
--  parseEntries ts ≫ foldM_ (insertEntry conn) tags_table

data CreateTables = CreateTables | CreateReCreateTables

data Options ε = Options { _dbFile :: File
                         , _inputFile :: 𝕄 File
                         , _createTables :: 𝕄 (Connection -> DoMock -> LoggingT (Log MockIOClass) (ExceptT ε IO) ())
                         }

dbFile ∷ Lens' (Options ε) File
dbFile = lens _dbFile (\ o f → o { _dbFile = f })

inputFile ∷ Lens' (Options ε) (𝕄 File)
inputFile = lens _inputFile (\ o f → o { _inputFile = f })

createTables ∷ Lens' (Options ε) (𝕄 (Connection → DoMock → LoggingT (Log MockIOClass) (ExceptT ε IO) ()))
createTables = lens _createTables (\ o c → o { _createTables = c })

optionsParser ∷ (AsSQLiteError ε, AsTextualParseError ε) ⇒ Parser (Options ε)
optionsParser =
  let createTables = flag' (flip buildTables NoReCreateTables) -- CreateTables
                       (mconcat [ short 'C', long "create-tables"
                                , help "create tables"
                                ])
      reCreateTables = flag' (flip buildTables ReCreateTables) -- CreateReCreateTables
                       (mconcat [ short 'R', long "re-create-tables"
                                , help "delete and re-create tables"
                                ])
  in  Options ⊳ (argument readM $ metavar "SQLITE-DB")
              ⊵ optional (argument readM $ metavar "INPUT-FILE")
              ⊵ optional (createTables ∤ reCreateTables)

doMain ∷ (AsIOError ε, AsTextualParseError ε, AsUsageError ε, AsSQLiteError ε) ⇒
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
      case opts ⊣ createTables of
        𝕹        → return ()
        𝕵 create → create c mck
      tags_table ← getTagsTable c
      parseEntries ts ≫ foldM_ (\ tgs e → insertEntry c tgs e mck) tags_table


main ∷ IO ()
main =
  let desc ∷ 𝕋 = "manipulate a brianDB"
  in  getArgs ≫ stdMain desc optionsParser (doMain @UsageSQLiteFPIOTPError)

-- that's all, folks! ----------------------------------------------------------
