{-# LANGUAGE UnicodeSyntax #-}
module Brian.SQLite
  ( Column(Column)
  , ColumnDesc(..)
  , ColumnFlag(..)
  , ColumnName(..)
  , ColumnType(..)
  , Table(..)
  , TableFlag(..)
  , TableName
  , columnID
  , createTable
  , execute
  , execute_
  , fold
  , insertTableRows
  , type RowType
    --  , insertRow
  , insertTableRows_
  , query
  , query_
  , reCreateTable
  , withinTransaction
  ) where

import Base1T
import Debug.Trace ( traceShow )

-- base --------------------------------

import Control.Exception qualified as Exception

import Data.Foldable ( Foldable )
import Data.List     ( filter )
import Data.Proxy    ( Proxy(Proxy) )
import GHC.Exts      ( IsString(fromString) )

-- logs-plus ---------------------------

import Log ( Log )

-- logging-effect ----------------------

import Control.Monad.Log ( MonadLog, Severity(Debug, Informational) )

-- mockio-log --------------------------

import MockIO.IOClass ( HasIOClass, IOClass(IOWrite) )
import MockIO.Log     ( DoMock, HasDoMock, mkIOLME )

-- safe-exceptions ---------------------

import Control.Exception.Safe ( bracketWithError )

-- sqlite-simple -----------------------

import Database.SQLite.Simple qualified as SQLite

import Database.SQLite.Simple ( Connection, FormatError, FromRow, Query(Query),
                                ResultError, SQLError, ToRow )

-- text --------------------------------

import Data.Text qualified as Text

-- text-printer ------------------------

import Text.Printer qualified as P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.Exceptions  ( catches )
import Brian.SQLiteError ( AsSQLiteError, SQuError, toAsSQLiteError )

--------------------------------------------------------------------------------

data ColumnType = CTypeText | CTypeInteger deriving (Show)

--------------------

instance Printable ColumnType where
  print CTypeText    = P.text "TEXT"
  print CTypeInteger = P.text "INTEGER"

------------------------------------------------------------

newtype ColumnName = ColumnName { unColumnName :: 𝕋 }
  deriving newtype (Eq, IsString, Ord, Show)

instance Printable ColumnName where print = P.text ∘ unColumnName

----------------------------------------

columnID ∷ ColumnName → 𝕋
columnID = (":"⊕) ∘ unColumnName

------------------------------------------------------------

data ColumnFlag = PrimaryKey | FlagUnique | NoInsert deriving (Eq, Show)

--------------------

instance Printable ColumnFlag where
  print PrimaryKey = P.text "PRIMARY KEY"
  print FlagUnique = P.text "UNIQUE"
  print NoInsert   = P.text ""

------------------------------------------------------------

data Column = Column { cname  :: ColumnName
                     , ctype  :: ColumnType
                     , cflags :: [ColumnFlag]
                     }

--------------------

instance Printable Column where
  print (Column { cname, ctype, cflags }) =
    let flags = ю $ (" " ⊕) ∘ toText ⊳ cflags
    in  P.text $ [fmt|%T %T %t|] cname ctype flags

------------------------------------------------------------

data TableFlag = OkayIfExists
               | ForeignKey [ColumnName]
  deriving (Eq, Show)

------------------------------------------------------------

newtype TableName = TableName { unTable :: 𝕋 }
  deriving newtype (IsString, Show)

instance Printable TableName where print = P.text ∘ unTable

------------------------------------------------------------

-- data ColumnTips = NoAttrs | NoInsert deriving (Eq)

------------------------------------------------------------

data ColumnDesc = ColumnDesc ColumnName ColumnType [ColumnFlag]
  deriving (Show)

cName ∷ ColumnDesc → 𝕋
cName (ColumnDesc n _ _) = unColumnName n

{- Which columns to use for insert -}
insertColumns ∷ Foldable φ ⇒ φ ColumnDesc → [𝕋]
insertColumns (toList → cols) =
  let noInsert (ColumnDesc _ _ flags) = NoInsert ∈ flags
  in  cName ⊳ filter (ﬧ ∘ noInsert) cols

{- columns description for CREATE TABLE statements -}
instance Printable ColumnDesc where
  print (ColumnDesc nm tp flgs) = P.text $
    let x = [fmt|%T %T %t|] nm tp (Text.intercalate " " $ filter (≢ "") $ toText ⊳ flgs)
    in traceShow ("x",x,"flgs",show flgs) $ x
------------------------------------------------------------

class Table α where
  type RowType α
  tName   ∷ Proxy α → TableName
  columns ∷ Proxy α → NonEmpty ColumnDesc
  createColumns ∷ Proxy α → [𝕋]
  createColumns = toText ⩺ toList ∘ columns

------------------------------------------------------------

execute ∷ ∀ ε ξ ω μ .
          (MonadIO μ, ToRow ξ, AsSQLiteError ε, MonadError ε μ, Printable ε,
           MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
          Severity → Connection → Query → ξ → DoMock → μ ()
execute sev conn sql r =
  let handlers = [ Exception.Handler $ return ∘ toAsSQLiteError @SQLError
                 , Exception.Handler $ return ∘ toAsSQLiteError @FormatError
                 ]
      io       = ((SQLite.execute conn sql r) `catches` handlers)
  in  mkIOLME sev IOWrite ([fmtT|sqlex %w|] sql) () io

----------------------------------------

execute_ ∷ ∀ ε ω μ . (MonadIO μ, AsSQLiteError ε, MonadError ε μ, Printable ε,
                      MonadLog (Log ω) μ, Default ω, HasIOClass ω,HasDoMock ω)⇒
           Severity → Connection → Query → DoMock → μ ()
execute_ sev conn sql =
  let handlers = [ Exception.Handler $ return ∘ toAsSQLiteError @SQLError
                 , Exception.Handler $ return ∘ toAsSQLiteError @FormatError
                 ]
      io       = ((SQLite.execute_ conn sql) `catches` handlers)
  in  mkIOLME sev IOWrite ([fmtT|sqlex %w|] sql) () io

----------------------------------------

query ∷ ∀ ε ξ χ ω μ .
        (MonadIO μ, ToRow ξ, FromRow χ,
         AsSQLiteError ε, Printable ε, MonadError ε μ,
         Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ) ⇒
        Severity → Connection → Query → ξ → [χ] → DoMock → μ [χ]
query sev conn sql r mock_value =
  let handlers = [ Exception.Handler $ return ∘ toAsSQLiteError @SQLError
                 , Exception.Handler $ return ∘ toAsSQLiteError @FormatError
                 , Exception.Handler $ return ∘ toAsSQLiteError @SQuError
                 , Exception.Handler $ return ∘ toAsSQLiteError @ResultError
                 ]
      io       = ((SQLite.query conn sql r) `catches` handlers)
  in  mkIOLME sev IOWrite ([fmtT|sqlqy %w|] sql) mock_value io

----------------------------------------

{- `query` that takes no parameters -}
query_ ∷ ∀ ε χ ω μ .
         (MonadIO μ, FromRow χ,
          AsSQLiteError ε, Printable ε, MonadError ε μ,
          MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
         Severity → Connection → Query → [χ] → DoMock → μ [χ]
query_ sev conn sql mock_value =
  let handlers = [ Exception.Handler $ return ∘ toAsSQLiteError @SQLError
                 , Exception.Handler $ return ∘ toAsSQLiteError @FormatError
                 , Exception.Handler $ return ∘ toAsSQLiteError @SQuError
                 , Exception.Handler $ return ∘ toAsSQLiteError @ResultError
                 ]
      io       = ((SQLite.query_ conn sql) `catches` handlers)
  in  mkIOLME sev IOWrite ([fmtT|sqlqy %w|] sql) mock_value io

----------------------------------------

{- Fold doesn't perform multiple, e.g., inserts; it folds the potentially many
   rows of results back to a single result. -}
fold ∷ ∀ ε ξ χ α ω μ .
       (MonadIO μ, ToRow ξ, FromRow χ,
        AsSQLiteError ε, Printable ε, MonadError ε μ,
        MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
       Severity → Connection → Query → ξ → α → (α → χ → IO α) → α → DoMock → μ α
fold sev conn sql r ini acc mock_value =
  let handlers = [ Exception.Handler $ return ∘ toAsSQLiteError @SQLError
                 , Exception.Handler $ return ∘ toAsSQLiteError @FormatError
                 , Exception.Handler $ return ∘ toAsSQLiteError @SQuError
                 , Exception.Handler $ return ∘ toAsSQLiteError @ResultError
                 ]
      io       = ((SQLite.fold conn sql r ini acc) `catches` handlers)
  in  mkIOLME sev IOWrite ([fmtT|sqlqy %w|] sql) mock_value io

----------------------------------------

{-
data Table = Table { _tname  :: TableName
                   , _tflags :: [TableFlag]
                   , _tcols  :: [Column]
                   }

tname ∷ Lens' Table TableName
tname = lens _tname (\ t n → t { _tname = n })

tflags ∷ Lens' Table [TableFlag]
tflags = lens _tflags (\ t fs → t { _tflags = fs })

tcols ∷ Lens' Table [Column]
tcols = lens _tcols (\ t cs → t { _tcols = cs })

-}

-- CREATE TABLE Entry (id INTEGER  PRIMARY KEY, title TEXT , medium TEXT , actresses TEXT , description TEXT );

createTable ∷ ∀ ε α ω μ . Table α ⇒
              (MonadIO μ, AsSQLiteError ε, MonadError ε μ, Printable ε,
               MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
              Connection → Proxy α → DoMock → μ ()
createTable conn p mck =
  let cols = Text.intercalate ", " $ createColumns p
      sql = fromString $ [fmt|CREATE TABLE %T (%t)|] (tName p) cols
  in  execute_ Informational conn sql mck

----------------------------------------

reCreateTable ∷ ∀ ε α ω μ . Table α ⇒
                (MonadIO μ, AsSQLiteError ε, MonadError ε μ, Printable ε,
                 MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
                Connection → Proxy α → DoMock → μ ()
reCreateTable conn p mck = do
  let sql = fromString $ [fmt|DROP TABLE %T|] (tName p)
  execute_ Informational conn sql mck
--  createTable conn (t & tflags ⊧ filter (≢ OkayIfExists)) mck
  createTable conn p mck

----------------------------------------

-- χ is the type of the returned row, e.g., (Only ID) for a single value
{-
insertRow ∷ ∀ ε α ξ χ ω μ . Table α ⇒
            (MonadIO μ, ToRow ξ, FromRow χ,
             AsSQLiteError ε, Printable ε, MonadError ε μ,
             MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
            Severity → Connection → α → 𝕄 𝕋 → ξ → [χ] → DoMock → μ [χ]
insertRow sev conn t extra r =
  let sql = fromString $ [fmt|INSERT INTO %T (%L) VALUES (%L)%T|]
                         (t ⊣ tname) (cname ⊳ t ⊣ tcols)
                         (const ("?"∷𝕋) ⊳ t ⊣ tcols) (maybe "" (" " ⊕) extra)
  in  query sev conn sql r
-}

----------------------------------------

withinTransaction ∷ (MonadIO μ, AsSQLiteError ε, Printable ε, MonadError ε μ,
                     Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ)⇒
                    Connection → DoMock → μ α → μ α
withinTransaction conn mck io = do
  execute_ Debug conn "BEGIN TRANSACTION" mck
  results ← io
  execute_ Debug conn "COMMIT TRANSACTION" mck
  return results

----------------------------------------

insertTableRows_ ∷ ∀ ε α β ω μ .
                   (MonadIO μ, Table α, ToRow (RowType α), FromRow β,
                    AsSQLiteError ε, Printable ε, MonadError ε μ,
                    Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ) ⇒
                   Severity → Proxy α → Connection → [RowType α] → 𝕋 → DoMock
                 → μ [(RowType α, [β])]
insertTableRows_ sev p conn rows extra mck = do
  let sql = traceShow ("c",columns p) $ Query $ [fmt|INSERT INTO %T (%L) VALUES (%L)%t%T|] (tName p)
                    (insertColumns ∘ toList $ columns p) (const ("?"∷𝕋) ⊳ (insertColumns ∘ toList $ columns p))
                    (if extra ≡ "" then "" else " ") extra
  forM rows $ \ row → (row,) ⊳ query sev conn sql row ф mck

----------------------------------------

insertTableRows ∷ ∀ ε α β ω μ .
                  (MonadIO μ, Table α, ToRow (RowType α), FromRow β,
                   AsSQLiteError ε, Printable ε, MonadError ε μ,
                   Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ) ⇒
                  Severity → Proxy α → Connection → [RowType α] → 𝕋 → DoMock
                → μ [(RowType α, [β])]
insertTableRows sev p conn rows extra mck =
  withinTransaction conn mck $ insertTableRows_ sev p conn rows extra mck
{-
  execute_ Debug conn "BEGIN TRANSACTION" mck
  let sql = Query $ [fmt|INSERT INTO %T (%L) VALUES (%L)%t%T|] (tName p)
                    (insertColumns ∘ toList $ columns p) (const ("?"∷𝕋) ⊳ (insertColumns ∘ toList $ columns p))
                    (if extra ≡ "" then "" else " ") extra
  results ← forM rows $ \ row → (row,) ⊳ query sev conn sql row ф mck

  execute_ Debug conn "COMMIT TRANSACTION" mck
  return results
-}

-- that's all, folks! ----------------------------------------------------------
