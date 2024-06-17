{-# LANGUAGE UnicodeSyntax #-}
module Brian
  ( main
  ) where

import Base1T

-- base --------------------------------

import Data.List.NonEmpty qualified as NonEmpty

import Control.Applicative ( optional )
import Control.Monad       ( foldM_, (=<<) )
import Data.Function       ( flip )
import Data.List           ( drop, maximum, zip )
import Data.List.NonEmpty  ( nonEmpty )
import Data.Monoid         ( mconcat )
import GHC.Exts            ( IsList(toList), IsString(fromString) )
import System.Environment  ( getArgs )

-- containers --------------------------

import Data.Map.Strict qualified as Map
import Data.Set        qualified as Set

-- fpath -------------------------------

import FPath.File      ( File(FileR) )
import FPath.Parseable ( readM )
import FPath.RelFile   ( relfile )

-- HTTP --------------------------------

import Network.HTTP ( getResponseBody, postRequestWithBody, simpleHTTP )

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Indexed ( itoList )

-- logging-effect ----------------------

import Control.Monad.Log ( LoggingT, MonadLog, Severity(Debug) )

-- logs-plus ---------------------------

import Log ( Log, infoT )

-- mockio-log --------------------------

import MockIO.IOClass ( HasIOClass )
import MockIO.Log     ( DoMock(DoMock, NoMock), HasDoMock, MockIOClass )

-- monadio-plus ------------------------

import MonadIO.OpenFile ( readFileUTF8Lenient )

-- optparse-applicative ----------------

import Options.Applicative ( Parser, argument, flag', help, long, metavar,
                             short )

-- sqlite-simple -----------------------

import Database.SQLite.Simple         ( Connection, FromRow, NamedParam((:=)),
                                        Only(Only), Query, SQLData,
                                        executeNamed, open, queryNamed, query_ )
import Database.SQLite.Simple.ToField ( ToField(toField) )

-- stdmain --------------------------------

import StdMain            ( stdMain )
import StdMain.UsageError ( AsUsageError, throwUsageT )

-- tagsoup -----------------------------

import Text.HTML.TagSoup ( Tag, parseTags )

-- text --------------------------------

import Data.Text ( pack )

-- textual-plus ------------------------

import TextualPlus.Error.TextualParseError ( AsTextualParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.BTag        ( BTag, unBTags )
import Brian.Entry       ( Entry, actresses, description, medium, parseEntries,
                           printEntry, recordNumber, tags, title )
import Brian.EntryData   ()
import Brian.ID          ( ID(ID, unID) )
import Brian.SQLite      ( Column(Column), ColumnFlag(FlagUnique, PrimaryKey),
                           ColumnName, ColumnType(CTypeInteger, CTypeText),
                           Table, TableFlag(ForeignKey, OkayIfExists), columnID,
                           createTable, execute_, reCreateTable )
import Brian.SQLiteError ( AsSQLiteError, UsageSQLiteFPIOTPError )

--------------------------------------------------------------------------------

openURL' ∷ String → String → IO String
openURL' x t = let content_type = "application/x-www-form-urlencoded"
                   postRequest  = postRequestWithBody x content_type t
               in  getResponseBody =<< simpleHTTP postRequest

brian ∷ MonadIO μ ⇒ μ String
brian = liftIO $ openURL' "http://brianspage.com/query.php" "description=gag"

infix 5 ~
(~) ∷ ToField τ ⇒ ColumnName → τ → (ColumnName,SQLData)
a ~ b = (a, toField b)

data ReCreateTables = ReCreateTables | NoReCreateTables

buildTables ∷ ∀ ε ω μ .
              (MonadIO μ, AsSQLiteError ε, AsTextualParseError ε,MonadError ε μ,
               MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
              Connection → ReCreateTables → DoMock → μ ()
buildTables conn recreate mck = do
  let create = case recreate of
                 ReCreateTables   → reCreateTable
                 NoReCreateTables → createTable
  create conn "Records" [ OkayIfExists ]
         [ Column "id"          CTypeInteger [PrimaryKey]
         , Column "title"       CTypeText    ф
         , Column "medium"      CTypeText    ф
         , Column "actresses"   CTypeText    ф
         , Column "tags"        CTypeText    ф
         , Column "description" CTypeText    ф
         ] mck
  create conn "Tags" [ OkayIfExists ]
         [ Column "id"          CTypeInteger [PrimaryKey]
         , Column "tag"         CTypeText    [FlagUnique]
         ] mck
  create conn "TagRef" [ OkayIfExists, ForeignKey ["recordid"] ]
         [ Column "recordid"    CTypeInteger [PrimaryKey]
         , Column "tagid"       CTypeInteger ф
         ] mck

type SQLLog α ε = Connection → DoMock
                → LoggingT (Log MockIOClass) (ExceptT ε IO) α

data Options ε = Options { _dbFile       :: File
                         , _inputFile    :: 𝕄 File
                         , _createTables :: 𝕄 (SQLLog () ε)
                         }

dbFile ∷ Lens' (Options ε) File
dbFile = lens _dbFile (\ o f → o { _dbFile = f })

inputFile ∷ Lens' (Options ε) (𝕄 File)
inputFile = lens _inputFile (\ o f → o { _inputFile = f })

createTables ∷
  Lens' (Options ε)
        (𝕄 (Connection → DoMock → LoggingT (Log MockIOClass) (ExceptT ε IO) ()))
createTables = lens _createTables (\ o c → o { _createTables = c })

optionsParser ∷ (AsSQLiteError ε, AsTextualParseError ε) ⇒ Parser (Options ε)
optionsParser =
  let create_tables    = flag' (flip buildTables NoReCreateTables)
                               (mconcat [ short 'C', long "create-tables"
                                        , help "create tables"
                                        ])
      re_create_tables = flag' (flip buildTables ReCreateTables)
                               (mconcat [ short 'R', long "re-create-tables"
                                        , help "delete and re-create tables"
                                        ])
  in  Options ⊳ (argument readM $ metavar "SQLITE-DB")
              ⊵ optional (argument readM $ metavar "INPUT-FILE")
              ⊵ optional (create_tables ∤ re_create_tables)

doMain ∷ (AsIOError ε, AsTextualParseError ε, AsUsageError ε, AsSQLiteError ε) ⇒
         DoMock → (Options ε) → LoggingT (Log MockIOClass) (ExceptT ε IO) ()
doMain mck opts = do
  case mck of
    DoMock → throwUsageT "dry-run not yet implemented"
    NoMock → return ()

  conn ← case opts ⊣ dbFile of
           FileR r | r ≡ [relfile|-|] → return 𝕹
           x                          → liftIO $ 𝕵 ⊳ open (toString x)
  t    ← case opts ⊣ inputFile of
           𝕵 f → readFileUTF8Lenient f
           𝕹   → pack ⊳ brian

  let ts ∷ [Tag 𝕋] = parseTags t

  case conn of
    𝕹   → parseEntries ts ≫ mapM_ printEntry
    𝕵 c → do
      case opts ⊣ createTables of
        𝕹        → return ()
        𝕵 create → create c mck
      tags_table ← getTagsTable c
      parseEntries ts ≫ foldM_ (\ tgs e → insertEntry c tgs e mck) tags_table


main ∷ IO ()
main =
  let desc ∷ 𝕋 = "manipulate a brianDB"
  in  getArgs ≫ stdMain desc optionsParser (doMain @UsageSQLiteFPIOTPError)

-- that's all, folks! ----------------------------------------------------------
