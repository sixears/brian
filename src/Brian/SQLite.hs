{-# LANGUAGE UnicodeSyntax #-}
module Brian.SQLite
  ( Column(Column)
  , ColumnFlag(..)
  , ColumnName
  , ColumnType(..)
  , Table(Table)
  , TableFlag(..)
  , TableName
  , columnID
  , createTable
  , execute_
  , fold
  , insertRow
  , query
  , reCreateTable
  ) where

import Base1T

-- base --------------------------------

import Data.List ( filter )
import GHC.Exts  ( IsString(fromString) )

-- logs-plus ---------------------------

import Log ( Log )

-- logging-effect ----------------------

import Control.Monad.Log ( MonadLog, Severity(Informational) )

-- mockio-log --------------------------

import MockIO.IOClass ( HasIOClass, IOClass(IOWrite) )
import MockIO.Log     ( DoMock, HasDoMock, mkIOLME )

-- safe-exceptions ---------------------

import Control.Exception qualified as Exception

-- sqlite-simple -----------------------

import Database.SQLite.Simple qualified as SQLite

import Database.SQLite.Simple ( Connection, FormatError, FromRow, Query,
                                ResultError, SQLError, ToRow )

-- text --------------------------------

import Data.Text ( intercalate )

-- text-printer ------------------------

import Text.Printer qualified as P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.Exceptions  ( catches )
import Brian.SQLiteError ( AsSQLiteError, SQuError, toAsSQLiteError )

--------------------------------------------------------------------------------

data ColumnType = CTypeText | CTypeInteger

------------------------------------------------------------

newtype ColumnName = ColumnName { unColumnName :: 𝕋 }
  deriving newtype (Eq, IsString, Ord, Show)

instance Printable ColumnName where print = P.text ∘ unColumnName

--------------------

instance Printable ColumnType where
  print CTypeText    = P.text "TEXT"
  print CTypeInteger = P.text "INTEGER"

----------------------------------------

columnID ∷ ColumnName → 𝕋
columnID = (":"⊕) ∘ unColumnName

------------------------------------------------------------

data ColumnFlag = PrimaryKey | FlagUnique

--------------------

instance Printable ColumnFlag where
  print PrimaryKey = P.text "PRIMARY KEY"
  print FlagUnique = P.text "UNIQUE"

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

{-
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
-}

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
         MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
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

createTable ∷ ∀ ε ω μ .
              (MonadIO μ, AsSQLiteError ε, MonadError ε μ, Printable ε,
               MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
              Connection → Table → DoMock → μ ()
createTable conn t mck =
  let exists = if OkayIfExists ∈ (t ⊣ tflags) then "IF NOT EXISTS " else ""
      columns = intercalate ", " $ toText ⊳ (t ⊣ tcols)
      sql = fromString $ [fmt|CREATE TABLE %t%T (%t)|] exists (t⊣tname) columns
  in  execute_ Informational conn sql mck

----------------------------------------

reCreateTable ∷ ∀ ε ω μ .
                (MonadIO μ, AsSQLiteError ε, MonadError ε μ, Printable ε,
                 MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
                Connection → Table → DoMock → μ ()
reCreateTable conn t mck = do
  let exists = if OkayIfExists ∈ (_tflags t) then "IF EXISTS " else ""
      sql    = fromString $ [fmt|DROP TABLE %s%T|] exists (_tname t)
  execute_ Informational conn sql mck
  createTable conn (t & tflags ⊧ filter (≢ OkayIfExists)) mck

----------------------------------------

-- χ is the type of the returned row, e.g., (Only ID) for a single value
insertRow ∷ ∀ ε ξ χ ω μ .
            (MonadIO μ, ToRow ξ, FromRow χ,
             AsSQLiteError ε, Printable ε, MonadError ε μ,
             MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
            Severity → Connection → Table → 𝕄 𝕋 → ξ → [χ] → DoMock → μ [χ]
insertRow sev conn t extra r =
  let sql = fromString $ [fmt|INSERT INTO %T (%L) VALUES (%L)%T|]
                         (t ⊣ tname) (cname ⊳ t ⊣ tcols) ((const ("?"∷𝕋)) ⊳ t ⊣ tcols) (maybe "" (" " ⊕) extra)
  in  query sev conn sql r

-- that's all, folks! ----------------------------------------------------------
