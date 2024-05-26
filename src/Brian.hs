{-# LANGUAGE UnicodeSyntax #-}
module Brian
  ( main
  ) where

import Base1

import Prelude ( Enum, undefined )

-- base --------------------------------

import Data.List.NonEmpty qualified as NonEmpty

import Control.Applicative ( optional )
import Control.Monad       ( foldM_, (=<<) )
import Data.List           ( drop, filter, maximum, reverse, takeWhile, zip )
import Data.List.NonEmpty  ( nonEmpty )
import Data.Maybe          ( catMaybes, fromMaybe )
import GHC.Exts            ( IsList(toList), IsString(fromString) )
import System.Environment  ( getArgs )
import System.IO           ( putStrLn )
import Text.Read           ( readEither )

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
import Control.Lens.Setter  ( (<>~) )

-- logs-plus ---------------------------

import Log ( Log, infoT )

-- logging-effect ----------------------

import Control.Monad.Log ( LoggingT, MonadLog )

-- mockio-log --------------------------

import MockIO.Log ( DoMock(DoMock), MockIOClass )

-- monadio-plus ------------------------

import MonadIO.OpenFile ( readFileUTF8Lenient )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens ( (⊩) )

-- neat-interpolation ------------------

import NeatInterpolation ( trimming )

-- optparse-applicative ----------------

import Options.Applicative ( Parser, argument, metavar )

-- safe-exceptions ---------------------

import Control.Exception.Safe ( mask, onException )

-- sqlite-simple -----------------------

import Database.SQLite.Simple         ( Connection, FromRow, NamedParam((:=)),
                                        Only(Only), Query, SQLData,
                                        ToRow(toRow), executeNamed, execute_,
                                        open, queryNamed, query_ )
import Database.SQLite.Simple.ToField ( ToField(toField) )

-- stdmain --------------------------------

import StdMain            ( stdMain )
import StdMain.UsageError ( AsUsageError, UsageFPIOTPError, throwUsageT )

-- tagsoup -----------------------------

import Text.HTML.TagSoup ( Tag, innerText, parseTags, partitions, (~/=), (~==) )

-- text --------------------------------

import Data.Text ( breakOn, intercalate, pack, splitOn, stripPrefix, unpack,
                   unwords, words )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus                         ( tparse )
import TextualPlus.Error.TextualParseError ( AsTextualParseError,
                                             throwAsTextualParseError )

-- word-wrap ---------------------------

import Text.Wrap ( FillStrategy(FillIndent), WrapSettings(fillStrategy),
                   defaultWrapSettings, wrapText )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.BTag   ( BTag )
import Brian.Entry  ( Entry, actresses, description, medium, parseEntries,
                      parseEntry, printEntry, recordNumber, tags, title )
import Brian.ID     ( ID(ID, unID), toℤ )
import Brian.Medium ( Medium )

--------------------------------------------------------------------------------

openURL' ∷ String → String → IO String
openURL' x t = let content_type = "application/x-www-form-urlencoded"
                   postRequest  = postRequestWithBody x content_type t
               in  getResponseBody =<< simpleHTTP postRequest

brian ∷ MonadIO μ ⇒ μ String
brian = liftIO $ openURL' "http://brianspage.com/query.php" "description=gag"


(≈) ∷ Tag 𝕋 → 𝕊 → 𝔹
(≈) tag t = (~==) tag ("<" ⊕ t ⊕ ">")

(≉) ∷ Tag 𝕋 → 𝕊 → 𝔹
(≉) tag t = (~/=) tag ("<" ⊕ t ⊕ ">")

text ∷ [Tag 𝕋] → 𝕋
text = unwords ∘ words ∘ innerText

entryParagraphs ∷ [Tag 𝕋] → [𝕋]
entryParagraphs p = filter (≢ "") $ text ⊳⊳ partitions (≈ "br")
                                 $ takeWhile (≉ "/blockquote") p

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

newtype Table = Table { unTable :: 𝕋 }
  deriving newtype (IsString, Show)

instance Printable Table where print = P.text ∘ unTable

newtype Column = Column { unColumn :: 𝕋 }
  deriving newtype (Eq, IsString, Ord, Show)

instance Printable Column where print = P.text ∘ unColumn

columnID ∷ Column → 𝕋
columnID = (":"⊕) ∘ unColumn

infix 5 ~
(~) ∷ ToField τ ⇒ Column → τ → (Column,SQLData)
a ~ b = (a, toField b)

newtype EntryData = EntryData { unEntryData :: Map.Map Column SQLData }
  deriving (Show)

instance IsList EntryData where
  type instance Item EntryData = (Column, SQLData)
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

iKeys ∷ Insert → [Column]
iKeys = Map.keys ∘ unEntryData ∘ NonEmpty.head ∘ view iEntryData

iQuery ∷ Insert → Query
iQuery i = fromString $
  let keys = iKeys i
  in  [fmt|INSERT INTO %T (%L) VALUES (%L)%T|] (i ⊣ iTable) keys
                                               (columnID ⊳ keys) (iEClause i)

iData ∷ Insert → [[NamedParam]]
iData =
  fmap (\ (k,v) → (columnID k := v)) ∘ itoList ∘ unEntryData
                                     ⩺ Base1.toList ∘ view iEntryData

insertSimple ∷ Connection → Insert → IO ()
insertSimple conn i = forM_ (iData i) $ executeNamed conn (iQuery i)

insertSimple' ∷ (MonadIO μ, FromRow r) ⇒ Connection → Insert → μ [[r]]
insertSimple' conn i = liftIO $ forM (iData i) $ queryNamed conn (iQuery i)

entryData ∷ Entry → Map.Map Column SQLData
entryData e =  [ "id"          ~ e ⊣ recordNumber
               , "title"       ~ e ⊣ title
               , "medium"      ~ e ⊣ medium
               , "actresses"   ~ intercalate "\v" (e ⊣ actresses)
               , "description" ~ intercalate "\v" (reverse $ e ⊣ description)
               , "tags"        ~ (""∷𝕋)
               ]

tagsInsert ∷ TagsTable → Entry → ([Insert], TagsTable)
tagsInsert tgs e =
  let tgs_max = maximum $ ID 0 : Map.elems tgs
      tg_new = Set.difference (fromList $ e ⊣ tags) (bTags tgs)
      tg_insert ∷ [(BTag,ID)]
      tg_insert = zip (Base1.toList tg_new) (drop 1 [tgs_max..])

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

withTransactionPrivate ∷ MonadIO μ ⇒ Connection → IO a → μ a
withTransactionPrivate conn action =
  liftIO $ mask $ \restore -> do
    begin
    r <- restore action `onException` rollback
    commit
    return r
  where
    begin    = execute_ conn $ "BEGIN TRANSACTION"
    commit   = execute_ conn $ "COMMIT TRANSACTION"
    rollback = execute_ conn $ "ROLLBACK TRANSACTION"

insertEntry ∷ (MonadIO μ, Default ω, MonadLog (Log ω) μ) ⇒
              Connection → TagsTable → Entry → μ TagsTable
insertEntry conn tgs e = do
  liftIO ∘ execute_ conn $ "BEGIN TRANSACTION"
  let insert = entryInsert e
      name  = fromMaybe "NO-TITLE" $ e ⊣ title
  tgs' ← insertSimple' conn insert ≫ \ case
    [[Only (n :: ID)]] → do
      infoT $ [fmt|inserted %d (%t)|] (unID n) name
      insertTags conn tgs e n
    _ → infoT ([fmt|no insert of %t|] name) ⪼ return tgs
  liftIO ∘ execute_ conn $ "COMMIT TRANSACTION"
  -- execute_ conn $ "ROLLBACK TRANSACTION" -- in emergency…
  return tgs'

insertTags ∷ MonadIO μ ⇒ Connection → TagsTable → Entry → ID → μ TagsTable
insertTags conn tgs e rid = liftIO $ do
  let (ins, tgs') = tagsInsert tgs e
  forM_ ins $ insertSimple conn
  case nonEmpty (e ⊣ tags) of
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

buildTables ∷ AsTextualParseError ε ⇒
              Connection → [Tag 𝕋] → LoggingT (Log MockIOClass) (ExceptT ε IO) ()
buildTables conn ts = do
  tags_table ← getTagsTable conn
  makeTable conn
  parseEntries ts ≫ foldM_ (insertEntry conn) tags_table

data Options = Options { _dbFile    :: File
                       , _inputFile :: 𝕄 File
                       }

dbFile ∷ Lens' Options File
dbFile = lens _dbFile (\ o f → o { _dbFile = f })

inputFile ∷ Lens' Options (𝕄 File)
inputFile = lens _inputFile (\ o f → o { _inputFile = f })

optionsParser ∷ Parser Options
optionsParser = Options ⊳ (argument readM $ metavar "SQLITE-DB")
                        ⊵ optional (argument readM $ metavar "INPUT-FILE")

doMain ∷ (AsIOError ε, AsTextualParseError ε, AsUsageError ε) ⇒
         DoMock → Options → LoggingT (Log MockIOClass) (ExceptT ε IO) ()
doMain do_mock opts = do
  if do_mock ≡ DoMock then throwUsageT "dry-run not yet implemented" else return ()
  conn ← case opts ⊣ dbFile of -- sequence $ liftIO . open ∘ toString ⊳ opts ⊣ dbFile
           FileR r | r ≡ [relfile|-|] → return 𝕹
           x                          → liftIO $ 𝕵 ⊳ open (toString x)
  t    ← case opts ⊣ inputFile of
           𝕵 f → readFileUTF8Lenient f
           𝕹   → pack ⊳ brian

  let ts ∷ [Tag 𝕋] = parseTags t

  case conn of
    𝕹   → parseEntries ts ≫ mapM_ printEntry
    𝕵 c → buildTables c ts

main ∷ IO ()
main =
  let desc ∷ 𝕋 = "manipulate a brianDB"
  in  getArgs ≫ stdMain desc optionsParser (doMain @UsageFPIOTPError)

-- that's all, folks! ----------------------------------------------------------
