{-# LANGUAGE UnicodeSyntax #-}
module Brian
  ( main
  ) where

import Base1

import Prelude ( Enum, error, undefined )

-- base --------------------------------

import Data.List.NonEmpty qualified as NonEmpty

import Control.Monad      ( foldM_, (=<<) )
import Data.List          ( drop, filter, maximum, reverse, takeWhile, zip )
import Data.List.NonEmpty ( nonEmpty )
import Data.Maybe         ( catMaybes, fromJust )
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

-- HTTP --------------------------------

import Network.HTTP ( getResponseBody, postRequestWithBody, simpleHTTP )

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Indexed ( itoList )
import Control.Lens.Setter  ( (<>~) )

-- logs-plus ---------------------------

import Log ( Log )

-- logging-effect ----------------------

import Control.Monad.Log ( LoggingT )

-- mockio-log --------------------------

import MockIO.Log ( MockIOClass )

-- monaderror-plus ---------------------

import MonadError.IO.Error ( IOError )

-- monadio-plus ------------------------

import MonadIO.OpenFile ( readFileUTF8Lenient )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens ( (⊩) )

-- natural -----------------------------

import Natural ( One )

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

-- parsers -----------------------------

import Text.Parser.Char        ( string )
import Text.Parser.Combinators ( choice )

-- stdmain --------------------------------

import StdMain            ( LogTIO, stdMain'' )
import StdMain.StdOptions ( DryRunLevel )
import StdMain.UsageError ( UsageFPIOTPError )

-- tagsoup -----------------------------

import Text.HTML.TagSoup ( Tag, innerText, parseTags, partitions, (~/=), (~==) )

-- text --------------------------------

import Data.Text ( breakOn, intercalate, pack, splitOn, stripPrefix, unpack,
                   unwords, words )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus                         ( TextualPlus(textual'), parseString,
                                             parseText, tparse )
import TextualPlus.Error.TextualParseError ( AsTextualParseError )

-- word-wrap ---------------------------

import Text.Wrap ( FillStrategy(FillIndent), WrapSettings(fillStrategy),
                   defaultWrapSettings, wrapText )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.Data ( BTag )


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

brian ∷ MonadIO μ ⇒ μ String
brian = liftIO $ openURL' "http://brianspage.com/query.php" "description=gag"


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

instance TextualPlus Medium where
  textual' = -- choice [ string "Soap Opera" ⋫ pure SoapOpera
                     {- , -}string "TV Series" ⋫ pure TVSeries
                    -- ]

{-
parseMedium ∷ 𝕋 → Medium
parseMedium "Soap Opera" = SoapOpera
parseMedium "TV Series"  = TVSeries
parseMedium t            = error $ [fmt|Unparsed medium: '%t'|] t
-}

data Entry = Entry { _recordNumber :: ID
                   , _title        :: 𝕄 𝕋
                   , _medium       :: 𝕄 Medium
                   , _actresses    :: [𝕋]
                   , _tags         :: [BTag]
                   , _description  :: [𝕋]
                   }
  deriving (Show)

instance ToRow Entry where
  toRow e = toRow (e ⊣ recordNumber, e ⊣ title)

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

parseBTags ∷ (MonadError ε η, AsTextualParseError ε) ⇒ 𝕋 → η [BTag]
parseBTags = sequence ∘ fmap tparse ∘ splitOn ", "

addEntryField ∷ (MonadError ε η, AsTextualParseError ε) ⇒ Entry → 𝕋 → η Entry
addEntryField e t =
  case second (stripPrefix ": ") $ (breakOn ":") t of
    ("Tags"       , 𝕵 t') → parseBTags t' ≫ return ∘ (e &) ∘ (tags <>~)
    ("Title"      , 𝕵 t') → return $ e & title       ⊩ t'
    ("Medium"     , 𝕵 t') → tparse t' ≫ return ∘ (e &) . (medium ⊩)
    ("Actress"    , 𝕵 t') → return $ e & actresses <>~ (splitOn ", " t')
    ("Description", 𝕵 t') → return $ e & description ⊧ (t' :)
    (_            , _   ) → return $ e & description ⊧ (t :)

addEntryFields ∷ (MonadError ε η, AsTextualParseError ε) ⇒ Entry → [𝕋] → η Entry
addEntryFields e ts = foldM addEntryField e ts

entryParagraphs ∷ [Tag 𝕋] → [𝕋]
entryParagraphs p = filter (≢ "") $ text ⊳⊳ partitions (≈ "br")
                                 $ takeWhile (≉ "/blockquote") p

parseEntry ∷ (MonadError ε η, AsTextualParseError ε) ⇒ [Tag 𝕋] → η Entry
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

insertEntry ∷ MonadIO μ ⇒ Connection → TagsTable → Entry → μ TagsTable
insertEntry conn tgs e = liftIO ∘ withTransaction conn $ do
  let insert = entryInsert e
  insertSimple' conn insert ≫ \ case
    [[Only (n :: ID)]] → do
      putStrLn $ show n
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

instance FromField ID where
  fromField f = case fromField @ℤ f of
    Ok n     → Ok $ fromℤ n
    Errors x → Errors x

getTagsTable ∷ MonadIO μ ⇒ Connection → μ TagsTable
getTagsTable conn = liftIO $ do
  let sql = "SELECT tag,id FROM Tags"
  rows ← query_ conn sql
  return $ Map.fromList rows

parseEntries ∷ (AsTextualParseError ε, MonadError ε η) ⇒ [Tag 𝕋] → η [Entry]
parseEntries ts = mapM parseEntry (partitions (≈ "blockquote") ts)

buildTables ∷ AsTextualParseError ε ⇒
              Connection → [Tag 𝕋] → LoggingT (Log MockIOClass) (ExceptT ε IO) ()
buildTables conn ts = do
  tags_table ← getTagsTable conn
  parseEntries ts ≫ foldM_ (insertEntry conn) tags_table

doMain ∷ AsTextualParseError ε ⇒
         DryRunLevel One → () → LoggingT (Log MockIOClass) (ExceptT ε IO) ()
doMain dry_run _ = do
  -- get these from the options
  args ← liftIO $ getArgs
  (t ∷ 𝕋, conn) ← case args of
    [f,db] → case (parse' @File f, parse' @File db) of
               (𝕽 f', 𝕽 db') → liftIO $ do
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
--    𝕹 → forM_ (partitions (≈ "blockquote") ts) (printEntry ∘ parseEntry)
    𝕵 conn' → buildTables conn' ts

main ∷ IO ()
main = stdMain'' "manipulate a brianDB" (pure ()) (doMain @UsageFPIOTPError)

-- that's all, folks! ----------------------------------------------------------
