{-# LANGUAGE UnicodeSyntax #-}
module Brian.BTag
  ( BTag(unBTag)
  , BTags(unBTags)
  , TagRefTable
  , TagsRow(TagsRow)
  , TagsTable
  , btags
  , insertTags
  , insertTags_
  , tagsRows
  ) where

import Base1T

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

import Database.SQLite.Simple           ( Connection, Only, SQLData(SQLText),
                                          ToRow(toRow) )
import Database.SQLite.Simple.FromField ( FromField(fromField) )
import Database.SQLite.Simple.ToField   ( ToField(toField) )

-- text --------------------------------

import Data.Text qualified as T

import Data.Text ( intercalate, pack )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus ( TextualPlus(textual') )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.ID          ( ID )
import Brian.SQLite      ( ColumnDesc(ColumnDesc),
                           ColumnFlag(FlagUnique, NoInsert, PrimaryKey),
                           ColumnType(CTypeInteger, CTypeText),
                           Table(columns, tName, type RowType),
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
             in  BTag ∘ pack ⊳ many (oneOf chars) <?> "BTag"

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
  print (BTags bs) = P.text $ intercalate ", " (toText ⊳ bs)

instance TextualPlus BTags where
  textual' = (BTags ⊳ textual' `sepBy` some (oneOf " .,<>:;()")) <?> "BTags"

btags ∷ [𝕋] → BTags
btags ts = BTags $ BTag ⊳ ts

------------------------------------------------------------

newtype TagsRow = TagsRow BTag

instance ToRow TagsRow where
  toRow (TagsRow btag) = [SQLText $ unBTag btag]

tagsRows ∷ BTags → [TagsRow]
tagsRows = TagsRow ⩺ unBTags

------------------------------------------------------------

data TagsTable

instance Table TagsTable where
  type instance RowType TagsTable = TagsRow
  tName   _ = "Tag"
  columns _ =    ColumnDesc "id" CTypeInteger [NoInsert,PrimaryKey]
            :| [ ColumnDesc "tag" CTypeText [FlagUnique] ]

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

insertTags_ ∷ (MonadIO μ, AsSQLiteError ε, Printable ε, MonadError ε μ,
               Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ) ⇒
              Connection → BTags → DoMock → μ [(TagsRow,[Only ID])]
insertTags_ conn tgs mck =
  let extra = T.intercalate " " [ "ON CONFLICT (id) DO NOTHING"
                                , "ON CONFLICT (tag) DO NOTHING"
                                , "RETURNING (id)"
                                ]
      pTags = Proxy ∷ Proxy TagsTable
  in  insertTableRows_ Informational pTags conn (tagsRows tgs) extra mck

----------------------------------------

insertTags ∷ (MonadIO μ, AsSQLiteError ε, Printable ε, MonadError ε μ,
              Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ) ⇒
             Connection → BTags → DoMock → μ [(TagsRow,[Only ID])]
insertTags conn tgs mck = withinTransaction conn mck $ insertTags_ conn tgs mck

-- that's all, folks! ----------------------------------------------------------
