{-# LANGUAGE UnicodeSyntax #-}
module Brian.SQLiteError
  ( AsSQLiteError(_SQLiteError)
  , UsageSQLiteFPIOTPError
  , toAsSQLiteError
  , toSQLiteError
  ) where

import Base1T

-- base --------------------------------

import Data.Function ( flip )
import GHC.Generics  ( Generic )

-- fpath -------------------------------

import FPath.Error.FPathError ( AsFPathError(_FPathError), FPathError,
                                FPathIOError )

-- sqlite-simple -----------------------

import Database.SQLite.Simple ( FormatError,
                                SQLError(sqlErrorContext, sqlErrorDetails) )

-- stdmain --------------------------------

import StdMain.UsageError ( AsUsageError(_UsageError), UsageError,
                            UsageFPIOTPError, UsageFPathIOError )

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus.Error.TextualParseError ( AsTextualParseError(_TextualParseError),
                                             TextualParseError )

--------------------------------------------------------------------------------

data SQLiteError = SQLE_SQLError { _sqlError       :: SQLError
                                 , _callstack_sqle :: CallStack
                                 }
                 | SQLE_FormatError { _formatError    :: FormatError
                                    , _callstack_fmte :: CallStack
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
  callstack = lens (\ case (SQLE_SQLError _ cs)    → cs
                           (SQLE_FormatError _ cs) → cs)
                   (\ e cs →
                      case e of (SQLE_SQLError e _)    → (SQLE_SQLError e cs)
                                (SQLE_FormatError e _) → (SQLE_FormatError e cs)
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
    P.text $ [fmt|%t [%t]|] (sqlErrorDetails e) (sqlErrorContext e)

------------------------------------------------------------

{-| combined @SQLiteError@, @UsageError@, @FPathError@, @IOError@,
    @TextualParseError@ -}
data UsageSQLiteFPIOTPError = USFPIOTPE_UFPIOTPE_ERROR UsageSQLiteFPIOTPError
                            | USFPIOTPE_SQLITE_ERROR SQLiteError
  deriving (Eq, Generic)

_USFPIOTPE_UFPIOTPE_ERROR ∷ Prism' UsageSQLiteFPIOTPError UsageSQLiteFPIOTPError
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

-- that's all, folks! ----------------------------------------------------------
