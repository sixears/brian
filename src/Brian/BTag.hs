{-# LANGUAGE UnicodeSyntax #-}

{- Tags, from Brian's database.  We use 'BTag' to avoid confusion with HTML
   tags from Text.HTML.TagSoup -}

module Brian.BTag
  ( BTag(unBTag)
  , BTags(unBTags)
  , TagRefTable
  , TagTable
  , TagsRow(TagsRow)
  , btags
  , insertEntryTags
  , insertEntryTags_
  , insertTagRefs
  , insertTagRefs_
  , insertTags
  , insertTags_
  , tagsRows
  ) where

import Base1T hiding ( toList )

-- base --------------------------------

import Data.Monoid ( Monoid )
import Data.Proxy  ( Proxy(Proxy) )
import GHC.Exts    ( IsString, toList )

-- logging-effect ----------------------

import Control.Monad.Log ( MonadLog, Severity(Informational) )

-- logs-plus ---------------------------

import Log ( Log )

-- mockio-log --------------------------

import MockIO.IOClass ( HasIOClass )
import MockIO.Log     ( DoMock, HasDoMock )

-- parsers -----------------------------

import Text.Parser.Char        ( oneOf )
import Text.Parser.Combinators ( sepBy, (<?>) )

-- sqlite-simple -----------------------

import Database.SQLite.Simple           ( Connection, Only, Query(Query),
                                          SQLData(SQLText), ToRow(toRow) )
import Database.SQLite.Simple.FromField ( FromField(fromField) )
import Database.SQLite.Simple.ToField   ( ToField(toField) )

-- text --------------------------------

import Data.Text qualified as T

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus ( TextualPlus(textual') )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.ID          ( ID(unID) )
import Brian.SQLite      ( ColumnDesc(ColumnDesc),
                           ColumnFlag(FlagUnique, NoInsert, PrimaryKey),
                           ColumnType(CTypeInteger, CTypeText),
                           Table(columns, tName, type RowType), execute,
                           insertTableRows_, withinTransaction )
import Brian.SQLiteError ( AsSQLiteError )

--------------------------------------------------------------------------------

newtype BTag = BTag { unBTag :: 𝕋 }
  deriving (Eq, Ord, Show)
  deriving newtype (IsString)

instance Printable BTag where
  print = P.text ∘ unBTag

instance TextualPlus BTag where
  textual' = let chars = "-_/ " ⊕ ['0'..'9'] ⊕ ['a'..'z'] ⊕ ['A'..'Z']
             in  BTag ∘ T.pack ⊳ many (oneOf chars) <?> "BTag"

instance ToField BTag where
  toField = toField ∘ unBTag

instance FromField BTag where
  fromField = BTag ⩺ fromField

------------------------------------------------------------

newtype BTags = BTags { unBTags :: [BTag] }
  deriving (Show)
  deriving newtype (Eq, Monoid, Semigroup)

instance IsList BTags where
  type instance Item BTags = BTag
  fromList = BTags
  toList   = unBTags

instance Printable BTags where
  print (BTags bs) = P.text $ T.intercalate ", " (toText ⊳ bs)

instance TextualPlus BTags where
  textual' = (BTags ⊳ textual' `sepBy` some (oneOf " .,<>:;()")) <?> "BTags"

btags ∷ [𝕋] → BTags
btags ts = BTags $ BTag ⊳ ts

------------------------------------------------------------

newtype TagsRow = TagsRow BTag
  deriving (Show)

instance ToRow TagsRow where
  toRow (TagsRow btag) = [SQLText $ unBTag btag]

tagsRows ∷ BTags → [TagsRow]
tagsRows = TagsRow ⩺ unBTags

------------------------------------------------------------

data TagTable

instance Table TagTable where
  type instance RowType TagTable = TagsRow
  tName   _ = "Tag"
  columns _ =    ColumnDesc "id" CTypeInteger [NoInsert,PrimaryKey]
            :| [ ColumnDesc "tag" CTypeText [FlagUnique] ]
----------------------------------------

insertTags_ ∷ (MonadIO μ, AsSQLiteError ε, Printable ε, MonadError ε μ,
               Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ) ⇒
              Connection → BTags → DoMock → μ [(TagsRow,[Only ID])]
insertTags_ conn tgs mck =
  let extra = T.intercalate " " [ "ON CONFLICT (id) DO NOTHING"
                                , "ON CONFLICT (tag) DO NOTHING"
                                , "RETURNING (id)"
                                ]
      pTags = Proxy ∷ Proxy TagTable
  in  insertTableRows_ Informational pTags conn (tagsRows tgs) extra mck

--------------------

insertTags ∷ (MonadIO μ, AsSQLiteError ε, Printable ε, MonadError ε μ,
              Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ) ⇒
             Connection → BTags → DoMock → μ [(TagsRow,[Only ID])]
insertTags conn tgs mck = withinTransaction conn mck $ insertTags_ conn tgs mck

------------------------------------------------------------

data TagsRefRow = TagsRefRow ID ID

instance ToRow TagsRefRow where
  toRow (TagsRefRow recordid tagid) = [toField recordid, toField tagid]

------------------------------------------------------------

data TagRefTable

instance Table TagRefTable where
  type instance RowType TagRefTable = TagsRefRow
  tName   _ = "TagRef"
  columns _ =    ColumnDesc "recordid" CTypeInteger []
            :| [ ColumnDesc "tagid" CTypeInteger [] ]

----------------------------------------

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

--------------------

insertTagRefs ∷ (MonadIO μ,
                 AsSQLiteError ε, Printable ε, MonadError ε μ,
                 HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
                Connection → ID → BTags → DoMock → μ ()
insertTagRefs conn n tgs mck =
  withinTransaction conn mck $ insertTagRefs_ conn n tgs mck

----------------------------------------

insertEntryTags_ ∷ (MonadIO μ,
                    AsSQLiteError ε, Printable ε, MonadError ε μ,
                    HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
                   Connection → ID → BTags → DoMock → μ ()
insertEntryTags_ conn n tgs mck = do
  _ ← insertTags_ conn tgs mck
  insertTagRefs_ conn n tgs mck

--------------------

insertEntryTags ∷ (MonadIO μ,
                   AsSQLiteError ε, Printable ε, MonadError ε μ,
                   HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
                  Connection → ID → BTags → DoMock → μ ()
insertEntryTags conn n tgs mck =
  withinTransaction conn mck $ insertEntryTags_ conn n tgs mck


-- that's all, folks! ----------------------------------------------------------
