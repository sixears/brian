{-# LANGUAGE UnicodeSyntax #-}
module Brian
  ( main
  ) where

import Base1

import Prelude ( Enum, error )

-- base --------------------------------

import Data.List.NonEmpty qualified as NonEmpty

import Control.Monad      ( foldM_, (=<<) )
import Data.List          ( drop, filter, maximum, reverse, takeWhile, zip )
import Data.List.NonEmpty ( nonEmpty )
import Data.Maybe         ( catMaybes )
import GHC.Exts           ( IsList(toList), IsString(fromString) )
import System.Environment ( getArgs )
import System.IO          ( putStrLn )
import Text.Read          ( Read(readPrec), readEither )

-- containers --------------------------

import Data.Map.Strict qualified as Map
import Data.Set        qualified as Set

-- fpath -------------------------------

import FPath.File      ( File )
import FPath.Parseable ( parse' )

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Indexed ( itoList )

-- monaderror-plus ---------------------

import MonadError.IO.Error ( IOError )

-- monadio-plus ------------------------

import MonadIO.OpenFile ( readFileUTF8Lenient )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens ( (⊩) )

-- neat-interpolation ------------------

import NeatInterpolation ( trimming )

-- sqlite-simple -----------------------

import Database.SQLite.Simple           ( Connection, FromRow, NamedParam((:=)),
                                          Only(Only), Query, SQLData,
                                          ToRow(toRow), executeNamed, execute_,
                                          open, queryNamed, query_,
                                          withTransaction )
import Database.SQLite.Simple.FromField ( FromField(fromField) )
import Database.SQLite.Simple.Ok        ( Ok(Errors, Ok) )
import Database.SQLite.Simple.ToField   ( ToField(toField) )

-- tagsoup -----------------------------

import Text.HTML.TagSoup ( Tag, innerText, parseTags, partitions, (~/=), (~==) )

-- text --------------------------------

import Data.Text ( breakOn, intercalate, pack, splitOn, stripPrefix, unpack,
                   unwords, words )

-- text-printer ------------------------

import Text.Printer qualified as P

-- word-wrap ---------------------------

import Text.Wrap ( FillStrategy(FillIndent), WrapSettings(fillStrategy),
                   defaultWrapSettings, wrapText )

-- HTTP --------------------------------

import Network.HTTP ( getResponseBody, postRequestWithBody, simpleHTTP )

--------------------------------------------------------------------------------

newtype ID = ID { unID :: ℕ }
  deriving (Enum, Eq, Ord, Show)

instance Read ID where
  readPrec = ID ⊳ readPrec

toℤ ∷ ID → ℤ
toℤ = fromIntegral ∘ unID

fromℤ ∷ ℤ → ID
fromℤ = ID ∘ fromIntegral

instance ToField ID where
  toField = toField ∘ toℤ

openURL' ∷ String → String → IO String
openURL' x t = let content_type = "application/x-www-form-urlencoded"
                   postRequest  = postRequestWithBody x content_type t
               in  getResponseBody =<< simpleHTTP postRequest

brian ∷ IO String
brian = openURL' "http://brianspage.com/query.php" "description=gag"


(≈) ∷ Tag 𝕋 → 𝕊 → 𝔹
(≈) tag t = (~==) tag ("<" ⊕ t ⊕ ">")

(≉) ∷ Tag 𝕋 → 𝕊 → 𝔹
(≉) tag t = (~/=) tag ("<" ⊕ t ⊕ ">")

text ∷ [Tag 𝕋] → 𝕋
text = unwords ∘ words ∘ innerText

data Medium = SoapOpera | TVSeries deriving (Show)

instance Printable Medium where
  print SoapOpera = P.text "Soap Opera"
  print TVSeries  = P.text "TV Series"

parseMedium ∷ 𝕋 → Medium
parseMedium "Soap Opera" = SoapOpera
parseMedium "TV Series"  = TVSeries
parseMedium t            = error $ [fmt|Unparsed medium: '%t'|] t

data Entry = Entry { _recordNumber :: ID
                   , _title        :: 𝕄 𝕋
                   , _medium       :: 𝕄 Medium
                   , _actresses    :: [𝕋]
                   , _tags         :: [BTag]
                   , _description  :: [𝕋]
                   }
  deriving (Show)

-- instance ToField ID where
--   toField = toField ∘ fromIntegral @_ @ℤ ∘ unID

instance ToRow Entry where
  toRow e = toRow ({- fromIntegral @_ @ℤ $ -} e ⊣ recordNumber, e ⊣ title)

recordNumber ∷ Lens' Entry ID
recordNumber = lens _recordNumber (\ e n → e { _recordNumber = n })

title ∷ Lens' Entry (𝕄 𝕋)
title = lens _title (\ e mt → e { _title = mt })

medium ∷ Lens' Entry (𝕄 Medium)
medium = lens _medium (\ e mm → e { _medium = mm })

actresses ∷ Lens' Entry ([𝕋])
actresses = lens _actresses (\ e as → e { _actresses = as })

tags ∷ Lens' Entry ([BTag])
tags = lens _tags (\ e as → e { _tags = as })

description ∷ Lens' Entry ([𝕋])
description = lens _description (\ e as → e { _description = as })

instance Printable Entry where
  print e =
    let fields = [ 𝕵 $ [fmt|Record      : %06d|] (toℤ $ e ⊣ recordNumber)
                 , [fmt|Title       : %t|] ⊳ (e ⊣ title)
                 , [fmt|Medium      : %T|] ⊳ (e ⊣ medium)
                 , case e ⊣ actresses of
                     [] → 𝕹
                     as → 𝕵 $ [fmt|Actresses   : %L|] as
                 , case e ⊣ tags of
                     [] → 𝕹
                     ts → 𝕵 $ [fmt|Tags        : %L|] ts
                 , case e ⊣ description of
                     [] → 𝕹
                     ts  → 𝕵 $ [fmt|Description :\n  %t|] (wrapText defaultWrapSettings { fillStrategy = FillIndent 2} 80 (unwords $ reverse ts))
                 ]
    in P.text $ intercalate "\n" (catMaybes fields)

mkEntry ∷ ID → Entry
mkEntry n = Entry { _recordNumber = n, _title = 𝕹, _medium = 𝕹
                  , _actresses = [], _description = [], _tags = [] }

addEntryField ∷ Entry → 𝕋 → Entry
addEntryField e t =
  case second (stripPrefix ": ") $ (breakOn ":") t of
    ("Title"      , 𝕵 t') → e & title ⊩ t' -- e { title = 𝕵 t' }
    ("Medium"     , 𝕵 t') → e & medium ⊩ parseMedium t'
    ("Actress"    , 𝕵 t') → e & actresses ⊧ (⊕ (splitOn ", " t'))
    ("Tags"       , 𝕵 t') → e & tags ⊧ (⊕ (BTag ⊳ splitOn ", " t'))
    ("Description", 𝕵 t') → e & description ⊧ (t' :)
    (_            , _   ) → e & description ⊧ (t :)

addEntryFields ∷ Entry → [𝕋] → Entry
addEntryFields e ts = foldl addEntryField e ts

entryParagraphs ∷ [Tag 𝕋] → [𝕋]
entryParagraphs p = filter (≢ "") $ text ⊳⊳ partitions (≈ "br")
                                 $ takeWhile (≉ "/blockquote") p

parseEntry ∷ [Tag 𝕋] → Entry
parseEntry ts =
  case breakOn ": " ⊳ (text ∘ pure ⊳ ts !! 1) of
    𝕵 ("Record number", n) →
      case readEither (drop 2 $ unpack n) of
        𝕷 err → error $ show (err, drop 2 (unpack n))
        𝕽 n'  → addEntryFields (mkEntry n') (entryParagraphs ts)
    _ → error $ "no record number!\n" ⊕ show ts

printEntry ∷ Entry → IO ()
printEntry ts = do
  putStrLn $ [fmt|%T\n|] ts

instance ToField Medium where
  toField m = toField (toText m)

makeTable ∷ Connection → IO ()
makeTable conn = do
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

insertSimple' ∷ FromRow r ⇒ Connection → Insert → IO [[r]]
insertSimple' conn i = forM (iData i) $ queryNamed conn (iQuery i)

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

insertEntry ∷ Connection → TagsTable → Entry → IO TagsTable
insertEntry conn tgs e = withTransaction conn $ do
  -- CR mpearce handle tags

--   simpleInsert conn (Table "Records") (entryData e)
--                     (𝕵 "ON CONFLICT (id) DO NOTHING RETURNING (id)")


--  let (inserts,tgs') = entryInserts tgs e
--  forM_ inserts $ insertSimple conn
--  return tgs'
  let insert = entryInsert e
--  [[Only (n :: ID)]] ← insertSimple conn insert
  insertSimple' conn insert ≫ \ case
    [[Only (n :: ID)]] → do
      putStrLn $ show n
      -- insert tags
      insertTags conn tgs e n
    _ → return tgs

insertTags ∷ Connection → TagsTable → Entry → ID → IO TagsTable
insertTags conn tgs e rid = do
  let (ins, tgs') = tagsInsert tgs e
  forM_ ins $ insertSimple conn
  case nonEmpty (e ⊣ tags) of
    𝕹 → return ()
    𝕵 tg_ids' → do
      let mkref t = ["recordid" ~ rid, "tagid" ~ Map.lookup t tgs']
      insertSimple conn $ Insert "TagRef" (mkref ⊳ tg_ids') 𝕹
  return tgs'

newtype BTag = BTag { unBTag :: 𝕋 }
  deriving (Eq, Ord, Show)

instance Printable BTag where
  print = P.text ∘ unBTag

instance ToField BTag where
  toField = toField ∘ unBTag

instance FromField BTag where
  fromField f = case fromField f of
    Ok t     → Ok $ BTag t
    Errors x → Errors x

instance FromField ID where
  fromField f = case fromField @ℤ f of
    Ok n     → Ok $ fromℤ n
    Errors x → Errors x

getTagsTable ∷ Connection → IO TagsTable
getTagsTable conn = do
  let sql = "SELECT tag,id FROM Tags"
  rows ← query_ conn sql
  return $ Map.fromList rows

main ∷ IO ()
main = do
  args ← getArgs
  (t ∷ 𝕋, conn) ← case args of
    [f,db] → case (parse' @File f, parse' @File db) of
               (𝕽 f', 𝕽 db') → do
                 conn ← open (toString db')
                 makeTable conn

                 (ѥ $ readFileUTF8Lenient @IOError f') ≫ \ case
                   𝕽 s → return (s,𝕵 conn)
                   𝕷 e → error $ show e

               (x,y) → error $ show (x,y)
    []  → (,𝕹) ⊳ pack ⊳ brian
    _   → error $ show args

  let ts ∷ [Tag 𝕋] = parseTags t

  case conn of
    𝕹 → forM_ (partitions (≈ "blockquote") ts) (printEntry ∘ parseEntry)
    𝕵 conn' → do
      tags_table ← getTagsTable conn'
      putStrLn $ show tags_table

      foldM_ (insertEntry conn') tags_table
             (parseEntry ⊳ partitions (≈ "blockquote") ts)

-- create table tags ( id INTEGER PRIMARY KEY, tag TEXT UNIQUE )
-- insert into tags (tag) VALUES ('tag1'),('tag2'),('tag4'),('tag3') ON CONFLICT (tag) DO NOTHING"
-- create table tagref ( recordid INTEGER, tagid INTEGER, FOREIGN KEY( recordid ) REFERENCES Records(id), FOREIGN KEY (tagid) REFERENCES Tags(Id))
-- SELECT id FROM Tags WHERE tag IN ('tag2','tag3')
-- "insert into Records (id, title) VALUES (5,'eez'), (9,'westh'),(7,'oeua')ON CONFLICT(id) DO NOTHING RETURNING (id)"
-- "INSERT INTO TagRef SELECT 1 AS recordid,id AS tagid from tags where tag in ('tag2','tag3')"

-- that's all, folks! ----------------------------------------------------------
