{-# LANGUAGE UnicodeSyntax #-}
module Brian.SQLiteError
  ( AsSQLiteError(_SQLiteError)
  , SQLiteError
  , SQuError
  , UsageSQLiteFPIOTPError
  , sqlMiscError
  , throwSQLMiscError
  , toAsSQLiteError
  , toSQLiteError
  ) where

import Base1T

-- base --------------------------------

import Data.Function ( flip )
import GHC.Generics  ( Generic )

-- fpath -------------------------------

import FPath.Error.FPathError ( AsFPathError(_FPathError) )

-- sqlite-simple -----------------------

import Database.SQLite.Simple qualified as SQLite

import Database.SQLite.Simple ( FormatError(fmtMessage, fmtParams, fmtQuery),
                                ResultError(errHaskellType, errMessage, errSQLType),
                                SQLError(sqlErrorContext, sqlErrorDetails) )

-- stdmain --------------------------------

import StdMain.UsageError ( AsUsageError(_UsageError), UsageFPIOTPError )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus.Error.TextualParseError ( AsTextualParseError(_TextualParseError) )

--------------------------------------------------------------------------------

{- | A wrapper to allow declaring SQLite.Error as an Exception -}
newtype SQuError = SQuError SQLite.Error
  deriving (Show)

instance Exception SQuError

------------------------------------------------------------

data SQLiteError = SQLE_SQLError { _sqlError  :: SQLError
                                 , _callstack :: CallStack
                                 }
                 | SQLE_FormatError { _formatError :: FormatError
                                    , _callstack   :: CallStack
                                    }
                 | SQLE_Error { _Error     :: SQuError
                              , _callstack :: CallStack
                              }
                 | SQLE_ResultError { _resultError :: ResultError
                                    , _callstack   :: CallStack
                                    }
                 | SQLE_MiscError { _txtError  :: 𝕋
                                  , _callstack :: CallStack
                                  }
  deriving (Generic, Show)

--------------------

instance Exception SQLiteError

--------------------

instance Eq SQLiteError where
  (SQLE_SQLError a _)    == (SQLE_SQLError b _)    = a == b
  (SQLE_FormatError a _) == (SQLE_FormatError b _) = a == b
  _                      == _                      = 𝕱

--------------------

instance HasCallstack SQLiteError where
  callstack = lens (\ case (SQLE_SQLError    _ cs) → cs
                           (SQLE_FormatError _ cs) → cs
                           (SQLE_Error       _ cs) → cs
                           (SQLE_ResultError _ cs) → cs
                           (SQLE_MiscError   _ cs) → cs
                   )
                   (\ e cs →
                      case e of (SQLE_SQLError    f _) → (SQLE_SQLError    f cs)
                                (SQLE_FormatError f _) → (SQLE_FormatError f cs)
                                (SQLE_Error       f _) → (SQLE_Error       f cs)
                                (SQLE_ResultError f _) → (SQLE_ResultError f cs)
                                (SQLE_MiscError   f _) → (SQLE_MiscError   f cs)
                   )

----------------------------------------

class ToSQLiteError ε where
  toSQLiteError ∷ ε → SQLiteError
  toAsSQLiteError ∷ AsSQLiteError γ ⇒ ε → γ
  toAsSQLiteError = (_SQLiteError #) ∘ toSQLiteError

instance ToSQLiteError SQLError where
  toSQLiteError = flip SQLE_SQLError callStack

instance ToSQLiteError FormatError where
  toSQLiteError = flip SQLE_FormatError callStack

instance ToSQLiteError SQuError where
  toSQLiteError = flip SQLE_Error callStack

instance ToSQLiteError ResultError where
  toSQLiteError = flip SQLE_ResultError callStack

------------------------------------------------------------

{-| prisms including @SQLiteError -}
class AsSQLiteError ε where
  _SQLiteError ∷ Prism' ε SQLiteError

--------------------

instance AsSQLiteError SQLiteError where
  _SQLiteError = id

--------------------

instance Printable SQLiteError where
  print (SQLE_SQLError e _) =
    P.text $ [fmt|¡SQLError! %t [%t]|] (sqlErrorDetails e) (sqlErrorContext e)
  print (SQLE_FormatError e _) =
    P.text $ [fmt|¡FormatError! %s: %w [%L]|] (fmtMessage e) (fmtQuery e) (fmtParams e)
  print (SQLE_Error e _) =
    P.text $ [fmt|¡SQLite.Error! %w|] e
  print (SQLE_ResultError e _) =
    P.text $ [fmt|¡ResultError! %s/%s (%s)|]
                 (errSQLType e) (errHaskellType e) (errMessage e)
  print (SQLE_MiscError e _) =
    P.text $ [fmt|¡SQLite.Error! %t|] e

------------------------------------------------------------

{-| combined @SQLiteError@, @UsageError@, @FPathError@, @IOError@,
    @TextualParseError@ -}
data UsageSQLiteFPIOTPError = USFPIOTPE_UFPIOTPE_ERROR UsageFPIOTPError
                            | USFPIOTPE_SQLITE_ERROR SQLiteError
  deriving (Eq, Generic)

_USFPIOTPE_UFPIOTPE_ERROR ∷ Prism' UsageSQLiteFPIOTPError UsageFPIOTPError
_USFPIOTPE_UFPIOTPE_ERROR =
  prism' (\ e → USFPIOTPE_UFPIOTPE_ERROR e)
         (\ case USFPIOTPE_UFPIOTPE_ERROR e → 𝕵 e; _ → 𝕹)

_USFPIOTPE_SQLITE_ERROR ∷ Prism' UsageSQLiteFPIOTPError SQLiteError
_USFPIOTPE_SQLITE_ERROR =
  prism' (\ e → USFPIOTPE_SQLITE_ERROR e)
         (\ case USFPIOTPE_SQLITE_ERROR e → 𝕵 e; _ → 𝕹)

--------------------

instance Exception UsageSQLiteFPIOTPError

--------------------

instance Show UsageSQLiteFPIOTPError where
  show (USFPIOTPE_SQLITE_ERROR e)      = show e
  show (USFPIOTPE_UFPIOTPE_ERROR    e) = show e

--------------------

instance AsSQLiteError UsageSQLiteFPIOTPError where
  _SQLiteError = _USFPIOTPE_SQLITE_ERROR

--------------------

instance AsUsageError UsageSQLiteFPIOTPError where
  _UsageError = _USFPIOTPE_UFPIOTPE_ERROR ∘ _UsageError

--------------------

instance AsTextualParseError UsageSQLiteFPIOTPError where
  _TextualParseError = _USFPIOTPE_UFPIOTPE_ERROR ∘ _TextualParseError

--------------------

instance AsFPathError UsageSQLiteFPIOTPError where
  _FPathError = _USFPIOTPE_UFPIOTPE_ERROR ∘ _FPathError

--------------------

instance AsIOError UsageSQLiteFPIOTPError where
  _IOError = _USFPIOTPE_UFPIOTPE_ERROR ∘ _IOError

--------------------

instance Printable UsageSQLiteFPIOTPError where
  print (USFPIOTPE_SQLITE_ERROR   e) = print e
  print (USFPIOTPE_UFPIOTPE_ERROR e) = print e

--------------------

instance HasCallstack UsageSQLiteFPIOTPError where
  callstack =
    let
      getter (USFPIOTPE_SQLITE_ERROR   e) = e ⊣ callstack
      getter (USFPIOTPE_UFPIOTPE_ERROR e) = e ⊣ callstack
      setter (USFPIOTPE_SQLITE_ERROR   e) cs =
        USFPIOTPE_SQLITE_ERROR (e & callstack ⊢ cs)
      setter (USFPIOTPE_UFPIOTPE_ERROR e) cs =
        USFPIOTPE_UFPIOTPE_ERROR (e & callstack ⊢ cs)
    in
      lens getter setter

----------------------------------------

sqlMiscError ∷ ∀ τ ε . (Printable τ, AsSQLiteError ε, HasCallStack) ⇒ τ → ε
sqlMiscError t = _SQLiteError # SQLE_MiscError (toText t) callStack

throwSQLMiscError ∷ ∀ τ ε ω η .
                    (Printable τ, AsSQLiteError ε, MonadError ε η) ⇒ τ → η ω
throwSQLMiscError t = throwError $ sqlMiscError t

-- that's all, folks! ----------------------------------------------------------
