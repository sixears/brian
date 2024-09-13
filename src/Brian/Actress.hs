{-# LANGUAGE UnicodeSyntax #-}
module Brian.Actress
  ( ActressRefTable
  , ActressTable
  , Actresses(Actresses)
  , insertActressRefs
  , insertActressRefs_
  , insertActresses
  , insertActresses_
  , insertEntryActresses
  , insertEntryActresses_
  , mkActresses
  , unActresses
  ) where

import Base1T hiding ( toList )

-- base --------------------------------

import Data.Proxy ( Proxy(Proxy) )
import GHC.Exts   ( IsString, toList )

-- logging-effect ----------------------

import Control.Monad.Log ( MonadLog, Severity(Informational) )

-- logs-plus ---------------------------

import Log ( Log )

-- mockio-log --------------------------

import MockIO.IOClass ( HasIOClass )
import MockIO.Log     ( DoMock, HasDoMock )

-- parsers -----------------------------

import Text.Parser.Char        ( alphaNum, char, noneOf, oneOf )
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

newtype Actress = Actress { unActress :: 𝕋 }
  deriving (Eq, Ord, Show)
  deriving newtype (IsString)

instance Printable Actress where
  print = P.text ∘ unActress

instance TextualPlus Actress where
  textual' = Actress ∘ T.pack ⊳ many (alphaNum ∤ oneOf " ") <?> "Actress"

instance ToField Actress where
  toField = toField ∘ unActress

instance FromField Actress where
  fromField = Actress ⩺ fromField

------------------------------------------------------------

newtype Actresses = Actresses { unActresses :: [Actress] }
  deriving newtype (Eq, Show)

instance IsList Actresses where
  type instance Item Actresses = Actress
  fromList = Actresses
  toList = unActresses

instance Printable Actresses where
  print = P.text ∘ T.intercalate ", " ∘ (toText ⩺ unActresses)

instance TextualPlus Actresses where
  textual' =
    Actresses ⊳         (Actress ∘ T.pack ⊳ some (noneOf ",\n"))
                `sepBy` (char ',' ⋪ many (char ' '))
             <?> "Actresses"

instance ToField Actresses where
  toField = toField ∘ T.intercalate ", " ∘ (toText ⩺ unActresses)

instance FromField Actresses where
  fromField = Actresses ∘ (Actress ⩺ T.splitOn ", ") ⩺ fromField

mkActresses ∷ [𝕋] → Actresses
mkActresses as = Actresses $ Actress ⊳ as

------------------------------------------------------------

newtype ActressesRow = ActressesRow Actress

instance ToRow ActressesRow where
  toRow (ActressesRow act) = [SQLText $ unActress act]

actressesRows ∷ Actresses → [ActressesRow]
actressesRows = ActressesRow ⩺ unActresses

------------------------------------------------------------

data ActressTable

instance Table ActressTable where
  type instance RowType ActressTable = ActressesRow
  tName   _ = "Actress"
  columns _ =    ColumnDesc "id" CTypeInteger [NoInsert,PrimaryKey]
            :| [ ColumnDesc "actress" CTypeText [FlagUnique] ]
----------------------------------------

insertActresses_ ∷ (MonadIO μ, AsSQLiteError ε, Printable ε, MonadError ε μ,
               Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ) ⇒
              Connection → Actresses → DoMock → μ [(ActressesRow,[Only ID])]
insertActresses_ conn acts mck =
  let extra = T.intercalate " " [ "ON CONFLICT (id) DO NOTHING"
                                , "ON CONFLICT (actress) DO NOTHING"
                                , "RETURNING (id)"
                                ]
      pActresses = Proxy ∷ Proxy ActressTable
      rows = actressesRows acts
  in  insertTableRows_ Informational pActresses conn rows extra mck

--------------------

insertActresses ∷ (MonadIO μ, AsSQLiteError ε, Printable ε, MonadError ε μ,
              Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ) ⇒
             Connection → Actresses → DoMock → μ [(ActressesRow,[Only ID])]
insertActresses conn acts mck =
  withinTransaction conn mck $ insertActresses_ conn acts mck

------------------------------------------------------------

data ActressesRefRow = ActressesRefRow ID ID

instance ToRow ActressesRefRow where
  toRow (ActressesRefRow recordid actressid) =
    [toField recordid, toField actressid]

------------------------------------------------------------

data ActressRefTable

instance Table ActressRefTable where
  type instance RowType ActressRefTable = ActressesRefRow
  tName   _ = "ActressRef"
  columns _ =    ColumnDesc "recordid" CTypeInteger []
            :| [ ColumnDesc "actressid" CTypeInteger [] ]

----------------------------------------

insertActressRefs_ ∷ (MonadIO μ,
                  AsSQLiteError ε, Printable ε, MonadError ε μ,
                  HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
                 Connection → ID → Actresses → DoMock → μ ()
insertActressRefs_ conn n acts =
  let sql =
        let insert = "INSERT INTO ActressRef (recordid, actressid)"
        in  Query $ [fmt|%t SELECT %d,id FROM Actress WHERE actress IN (%L)|]
                    insert (unID n) (const ("?"∷𝕋) ⊳ toList acts)
  in  execute @_ @[𝕋] Informational conn sql (toText ⊳ toList acts)

--------------------

insertActressRefs ∷ (MonadIO μ,
                 AsSQLiteError ε, Printable ε, MonadError ε μ,
                 HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
                Connection → ID → Actresses → DoMock → μ ()
insertActressRefs conn n acts mck =
  withinTransaction conn mck $ insertActressRefs_ conn n acts mck

--------------------

insertEntryActresses_ ∷ (MonadIO μ,
                    AsSQLiteError ε, Printable ε, MonadError ε μ,
                    HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
                   Connection → ID → Actresses → DoMock → μ ()
insertEntryActresses_ conn n acts mck = do
  _ ← insertActresses_ conn acts mck
  insertActressRefs_ conn n acts mck

--------------------

insertEntryActresses ∷ (MonadIO μ,
                   AsSQLiteError ε, Printable ε, MonadError ε μ,
                   HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
                  Connection → ID → Actresses → DoMock → μ ()
insertEntryActresses conn n acts mck =
  withinTransaction conn mck $ insertEntryActresses_ conn n acts mck

-- that's all, folks! ----------------------------------------------------------
