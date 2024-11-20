{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UnicodeSyntax   #-}

module Brian.Entry
  ( Entry(Entry, _actresses, _description, _entryDate, _episode, _medium, _recordNumber, _tags, _title)
  , EntryRow
  , EntryTable
  , actresses
  , description
  , entryRow
  , episode
  , insertEntry
  , medium
  , parseEntries
  , printEntry
  , readEntry
  , recordNumber
  , tags
  , title
  ) where

import Base1T

-- base --------------------------------

import Control.Applicative ( Alternative )
import Control.Monad.Fail  ( MonadFail )
import Data.Either         ( partitionEithers )
import Data.List           ( filter, takeWhile )
import Data.Maybe          ( catMaybes, fromMaybe )
import Data.Proxy          ( Proxy(Proxy) )
import System.IO           ( putStrLn )

-- lens --------------------------------

import Control.Lens.Getter ( view )

-- logging-effect ----------------------

import Control.Monad.Log ( MonadLog, Severity(Debug, Informational) )

-- log-plus ----------------------------

import Log ( Log )

-- mockio-log --------------------------

import MockIO.IOClass ( HasIOClass )
import MockIO.Log     ( DoMock(NoMock), HasDoMock, logio )

-- monadio-plus ------------------------

import MonadIO ( say )

-- natural -----------------------------

import Natural ( length )

-- parsers -----------------------------

import Text.Parser.Char        ( CharParsing, char, noneOf, string )
import Text.Parser.Combinators ( eof, sepBy, (<?>) )

-- sqlite-simple -----------------------

import Database.SQLite.Simple         ( Connection, Only(Only), Query(Query),
                                        SQLData(SQLInteger), ToRow(toRow) )
import Database.SQLite.Simple.ToField ( ToField(toField) )

-- tagsoup -----------------------------

import Text.HTML.TagSoup ( Tag, partitions )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus                         ( TextualPlus(textual'), parseTextM,
                                             tparse' )
import TextualPlus.Error.TextualParseError ( AsTextualParseError,
                                             throwAsTextualParseError )

-- text --------------------------------

import Data.Text qualified as T

-- trifecta ----------------------------

import Text.Trifecta.Result ( Result(Failure, Success) )

-- trifecta-plus -----------------------

import TrifectaPlus ( tParse )

-- word-wrap ---------------------------

import Text.Wrap ( FillStrategy(FillIndent), WrapSettings(fillStrategy),
                   defaultWrapSettings, wrapText )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.Day         qualified as Day
import Brian.Description qualified as Description

import Brian.Actress     ( Actresses, insertEntryActresses_, mkActresses )
import Brian.BTag        ( BTags, btags, insertEntryTags_ )
import Brian.Day         ( Day )
import Brian.Description ( Description(unDescription), more )
import Brian.Episode     ( Episode, EpisodeID(EpisodeID), EpisodeName, epID,
                           epName, epi )
import Brian.ID          ( ID(unID), toℤ )
import Brian.Medium      ( Medium )
import Brian.Parsers     ( whitespace )
import Brian.ShowSQL     ( ShowSQL(ShowSQL) )
import Brian.SQLite      ( ColumnDesc(ColumnDesc), ColumnFlag(PrimaryKey),
                           ColumnType(CTypeInteger, CTypeText),
                           Table(columns, tName, type RowType),
                           insertTableRows_, qry, query, sjoin, sqlFmt,
                           withinTransaction )
import Brian.SQLiteError ( AsSQLiteError, throwSQLMiscError )
import Brian.TagSoup     ( text, (≈), (≉) )
import Brian.Title       ( Title, unTitle )

--------------------------------------------------------------------------------

data Entry = Entry { _recordNumber :: ID
                   , _title        :: Title
                   , _medium       :: 𝕄 Medium
                   , _actresses    :: Actresses
                   , _tags         :: BTags
                   , _description  :: Description
                   , _episode      :: 𝕄 Episode
                   , _entryDate    :: Day
                   }
  deriving (Eq, Show)

recordNumber ∷ Lens' Entry ID
recordNumber = lens _recordNumber (\ e n → e { _recordNumber = n })

title ∷ Lens' Entry Title
title = lens _title (\ e t → e { _title = t })

medium ∷ Lens' Entry (𝕄 Medium)
medium = lens _medium (\ e mm → e { _medium = mm })

actresses ∷ Lens' Entry Actresses
actresses = lens _actresses (\ e as → e { _actresses = as })

tags ∷ Lens' Entry BTags
tags = lens _tags (\ e as → e { _tags = as })

description ∷ Lens' Entry Description
description = lens _description (\ e d → e { _description = d })

episode ∷ Lens' Entry (𝕄 Episode)
episode = lens _episode (\ e d → e { _episode = d })

entryDate ∷ Lens' Entry Day
entryDate = lens _entryDate (\ e d → e { _entryDate = d })

----------------------------------------

instance Printable Entry where
  print e =
    let wd = 80
        mfmt xs f = case xs of [] → 𝕹; _ →  𝕵 $ f xs
        wrapn i = wrapText defaultWrapSettings { fillStrategy=FillIndent i }
                           (fromIntegral wd)
        fields = [ 𝕵 $ [fmt|Record      : %06d|] (toℤ $ e ⊣ recordNumber)
                 , 𝕵 $ [fmt|EntryDate   : %T|] (e ⊣ entryDate)
                 , 𝕵 $ [fmt|Title       : %t|] (unTitle $ e ⊣ title)
                 , [fmt|Medium      : %T|] ⊳ (e ⊣ medium)
                 , [fmt|Episode     : %T|] ⊳ (toText ⊳ e ⊣ episode)
                 , 𝕵 $ [fmtT|Actresses   : %T|]  (e ⊣ actresses)
                 , mfmt (wrapn 14 ∘ toText $ e ⊣ tags) [fmt|Tags        : %t|]
                 , let descn = toText $ e ⊣ description
                   in  𝕵 $ [fmtT|Description : %t|]
                       (if length descn + 14 ≤ wd
                        then descn
                        else "\n  " ⊕ wrapn 2 (T.replace "\n" "\n\n  " descn))
                 ]
    in P.text $ T.intercalate "\n" (catMaybes fields)

------------------------------------------------------------

data EntryRow = EntryRow { _erRecordNumber :: ID
                         , _erTitle        :: Title
                         , _erMedium       :: 𝕄 Medium
                         , _erDescription  :: Description
                         , _erEpisodeID    :: EpisodeID
                         , _erEpisodeName  :: 𝕄 EpisodeName
                         , _erEntryDate    :: Day
                         }
  deriving (Show)

entryRow ∷ Day → Entry → EntryRow
entryRow d e = EntryRow (e ⊣ recordNumber)
                        (e ⊣ title)
                        (e ⊣ medium)
                        (e ⊣ description)
                        (maybe (EpisodeID []) (view epID) (e ⊣ episode))
                        (maybe 𝕹 (view epName) (e ⊣ episode))
                        d

instance ToRow EntryRow where
  toRow (EntryRow rn tt md ds epid epn ed) =
    toRow (rn, unTitle tt, md, toField ds, toField epid,toField epn,
           toField ed)

----------------------------------------

parseEithers ∷ Alternative ψ ⇒ ψ α → ψ β → ψ sep → ψ ([α], [β])
parseEithers l r n = partitionEithers ⊳ (𝕷 ⊳ l ∤ 𝕽 ⊳ r) `sepBy` n

instance TextualPlus Entry where
  textual' =
    let mkEntry (rn,tt,md,ac,dn,(gs,ds)) = do
          tgs ← ю ⊳ mapM (parseTextM "BTag*") gs
          (ep,dn') ← case tParse @Episode (T.unpack $ unDescription dn) of
            Success e → return (𝕵 e, Description.fromLines (T.pack ⊳ ds))
            Failure _ → return (𝕹, dn `more` (T.pack ⊳ ds))
          return $ Entry { _recordNumber = rn
                         , _title        = tt
                         , _medium       = 𝕵 md
                         , _actresses    = ac
                         , _description  = dn'
                         , _tags         = tgs
                         , _episode      = ep
                         , _entryDate    = Day.epoch
                         }
        ҕ ∷ ∀ α η . (TextualPlus α, MonadFail η, CharParsing η) ⇒ 𝕊 → η α
        ҕ t = let end = (pure () ⋪ char '\n') ∤ eof
              in  string (t ⊕ ":") ⋫ whitespace ⋫ textual' ⋪ whitespace ⋪ end
        restOfLine = many $ noneOf "\n"
    in ((,,,,,) ⊳ ҕ "Record number"
                ⊵ ҕ "Title"
                ⊵ ҕ "Medium"
                ⊵ ҕ "Actress"
                ⊵ ҕ @Description "Description"
                ⊵ parseEithers (T.pack ⊳ (string "Tags: " ⋫ restOfLine))
                               restOfLine (char '\n')
                <?> "Entry") ≫ mkEntry

----------------------------------------

parseEntry ∷ (MonadError ε η, AsTextualParseError ε) ⇒ [𝕋] → η Entry
parseEntry ts =
  case tparse' (T.intercalate "\n" ts) of
    𝕽 e   → return e
    𝕷 err → throwAsTextualParseError "no parse Entry"
                                     (toString err : (T.unpack ⊳ ts))

----------------------------------------

entryParagraphs ∷ [Tag 𝕋] → [𝕋]
entryParagraphs p =
  filter (≢ "") $ text ⊳⊳ (\ ts → takeWhile (≉"br") ts : partitions (≈ "br") ts)
                $ takeWhile (≉ "/blockquote") p

----------------------------------------

parseEntries ∷ (AsTextualParseError ε, MonadError ε η) ⇒ [Tag 𝕋] → η [Entry]
parseEntries ts =
  mapM parseEntry (entryParagraphs ⊳ partitions (≈ "blockquote") ts)

----------------------------------------

printEntry ∷ MonadIO μ ⇒ Entry → μ ()
printEntry ts = liftIO ∘ putStrLn $ [fmt|%T\n|] ts

----------------------------------------

data EntryTable

--------------------

instance Table EntryTable where
  type instance RowType EntryTable = EntryRow
  tName   _ = "Entry"
  columns _ =  ( ColumnDesc "id"          CTypeInteger [PrimaryKey] )
            :| [ ColumnDesc "title"       CTypeText []
               , ColumnDesc "medium"      CTypeText []
--               , ColumnDesc "actresses"   CTypeText []
               , ColumnDesc "description" CTypeText []
               , ColumnDesc "episodeid"   CTypeText []
               , ColumnDesc "episodename" CTypeText []
               , ColumnDesc "entrydate"   CTypeInteger []
               ]

------------------------------------------------------------

insertEntry_ ∷ ∀ ε ω μ .
               (MonadIO μ, Default ω, MonadLog (Log ω) μ,
                AsSQLiteError ε, Printable ε, MonadError ε μ,
                MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
               Connection → Day → Entry → DoMock → μ (𝕄 ID)
insertEntry_ conn d e mck = do
  let name  = e ⊣ title
  row_ids ← insertTableRows_ Informational (Proxy ∷ Proxy EntryTable) conn
                             [entryRow d e]
                             "ON CONFLICT (id) DO NOTHING RETURNING (id)" mck

  case row_ids of
    [(_, [Only (n ∷ ID)])] → do
      logio Informational ([fmtT|inserted %d (%T)|] (unID n) name) NoMock
      insertEntryTags_ conn n (e ⊣ tags) mck
      insertEntryActresses_ conn n (e ⊣ actresses) mck
      return $ 𝕵 n
    _ → return 𝕹

--------------------

insertEntry ∷ ∀ ε ω μ .
              (MonadIO μ, Default ω, MonadLog (Log ω) μ,
               AsSQLiteError ε, Printable ε, MonadError ε μ,
               MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
              Connection → Day → Entry → DoMock → μ (𝕄 ID)
insertEntry conn d e mck= withinTransaction conn mck $ insertEntry_ conn d e mck

----------------------------------------

readEntry ∷ ∀ ε ω μ .
            (MonadIO μ, Default ω, MonadLog (Log ω) μ,
             AsSQLiteError ε, Printable ε, MonadError ε μ,
             MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
            Connection → ID → ShowSQL → DoMock → μ (𝕄 Entry)
readEntry conn eid show_sql mck = do

  let sql = let actrsss ∷ [𝕋] = -- sjoin
                  [ "SELECT ActressRef.recordid,"
                  , "       GROUP_CONCAT(Actress.actress, ', ') as actresses"
                  , "  FROM ActressRef, Actress"
                  , " WHERE ActressRef.actressid = Actress.id"
                  , " GROUP BY ActressRef.recordid"
                  ]
                tgss ∷ [𝕋] = -- sjoin
                  [ "SELECT TagRef.recordid,"
                  , "       GROUP_CONCAT(Tag.tag, ', ') as tags"
                  , "  FROM TagRef,Tag"
                  , " WHERE TagRef.tagid = Tag.id"
                  , " GROUP BY TagRef.recordid"
                  ]
                left_joins = [ (T.unlines actrsss, "Actresses", ("recordid", "Entry.id"))
                             , (T.unlines tgss, "Tags", ("recordid", "Entry.id"))
                             ]
                joinsql =
                  [ [fmt|LEFT JOIN (%t) AS %t ON %t.%t = %t|]
                      expr tab_as tab_as lhs rhs
                    | (expr, tab_as, (lhs,rhs)) ← left_joins ]
                tables ∷ [𝕋] = [ "Entry" ]
                fields ∷ [𝕋] = [ "title", "medium", "description", "episodeid"
                               , "episodename", "entrydate"
                               , "Actresses.actresses", "Tags.tags"
                               ]
                wheres ∷ [𝕋] = [ "Entry.id = ?"
                               , "Actresses.recordid = Entry.id"
                               ]
                sqlt = [fmt|SELECT %L FROM %L %t WHERE %t|]
                            fields tables (T.unwords joinsql)
                            (T.intercalate " AND " wheres)
            in  {- do when (show_sql ≡ ShowSQL) (say $ sqlFmt sql [eid]) -}
                   {- Query -} {- $ [fmt|SELECT %L FROM %L %t WHERE %t|]
                        fields tables (T.unwords joinsql)
                        (T.intercalate " AND " wheres) -} [sqlt]
  query Informational conn 𝕹 (qry sql [SQLInteger ∘ fromIntegral $ toℤ eid]) [] mck ≫ \ case
    []                    → return 𝕹

    row@[(ttle,mdm,desc,epid,epname,edate,actrsss∷𝕋,tagss ∷ 𝕄 𝕋)] → do
      logio Debug ([fmtT|entry row: %w|] row) NoMock
      let tgs = btags $ T.splitOn ", " (fromMaybe "" tagss)
      return ∘ 𝕵 $ Entry eid ttle (𝕵 mdm) (mkActresses $ T.splitOn ", " actrsss)
                         tgs desc (epi epid epname) edate

    xs                    →
      throwSQLMiscError $ [fmtT|too many (%d) entries found for %d|]
                          (unID eid) (length xs)

-- that's all, folks! ----------------------------------------------------------
