{-# LANGUAGE UnicodeSyntax #-}
module Brian.DBEntryPreFilter
  ( DBEntryPreFilter
  , DBEntryPreFilterItem(DBEntryEntryDateFilter, DBEntryTitleFilter)
  , actressFilter
  , conj
  , dateFilter
  , descFilter
  , gFilt
  , null
  , tests
  , titleFilter
  , whereClause
  ) where

import Base1T
import Prelude ( (*) )

-- base --------------------------------

import Data.List.NonEmpty ( unzip )
import Text.Read          ( read )

-- optparse-plus -----------------------

import OptParsePlus ( OptReader(readM) )

-- parsers -----------------------------

import Text.Parser.Char ( char, digit )

-- parser-plus -------------------------

import ParserPlus ( boundedDoubledChars, parens )

-- sqlite-simple -----------------------

import Database.SQLite.Simple ( SQLData, ToRow(toRow) )

-- text --------------------------------

import Data.Text qualified as T

-- textual-plus ------------------------

import TextualPlus ( TextualPlus(textual'), parenthesize, surround )

-- time --------------------------------

import Data.Time.Calendar       ( addDays )
import Data.Time.Clock          ( getCurrentTime, utctDay )
import Data.Time.Format.ISO8601 ( iso8601Show )

-- trifecta-plus -----------------------

import TrifectaPlus ( testParse )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.PredicateFilter qualified as PredicateFilter
import Brian.TrifectaPlus qualified

import Brian.PredicateFilter ( PredicateFilter(EF_Conj, EF_Disj, EF_NotPred, EF_Pred) )

--------------------------------------------------------------------------------

data DBEntryPreFilterItem = DBEntryTitleFilter 𝕋
                          | DBEntryActressFilter 𝕋
                          | DBEntryDescFilter 𝕋
                          | DBEntryTagFilter 𝕋
                          | DBEntryMediumFilter 𝕋
                          | DBEntryEpNameFilter 𝕋
                          | DBEntryEpIDFilter 𝕋
                          | DBEntryEntryDateFilter ℕ
                          | DBEntryNullFilter
  deriving (Eq, Show)

--------------------

instance TextualPlus DBEntryPreFilterItem where
  textual' =
    let braced        = T.pack ⊳ boundedDoubledChars '{' '}'
        bracedGlobbed = surround "%" ⊳ braced
    in    char 't' ⋫ (DBEntryTitleFilter ⊳ bracedGlobbed)
        ∤ char 'T' ⋫ (DBEntryTitleFilter ⊳ braced)
        ∤ char 'a' ⋫ (DBEntryActressFilter ⊳ bracedGlobbed)
        ∤ char 'A' ⋫ (DBEntryActressFilter ⊳ braced)
        ∤ char 'd' ⋫ (DBEntryDescFilter ⊳ bracedGlobbed)
        ∤ char 'D' ⋫ (DBEntryDescFilter ⊳ braced)
        ∤ char 'm' ⋫ (DBEntryMediumFilter ⊳ bracedGlobbed)
        ∤ char 'M' ⋫ (DBEntryMediumFilter ⊳ braced)
        ∤ char 'e' ⋫ (DBEntryEpNameFilter ⊳ bracedGlobbed)
        ∤ char 'E' ⋫ (DBEntryEpNameFilter ⊳ braced)
        ∤ char 'i' ⋫ (DBEntryEpIDFilter ⊳ bracedGlobbed)
        ∤ char 'I' ⋫ (DBEntryEpIDFilter ⊳ braced)
        ∤ char 'y' ⋫ (DBEntryEntryDateFilter ⊳ parens (read ⊳ some digit))
        ∤ char 'g' ⋫ (DBEntryTagFilter ⊳ bracedGlobbed)
        ∤ char 'G' ⋫ (DBEntryTagFilter ⊳ braced)

-- medium,description,episodeid,episodename

--------------------

instance OptReader DBEntryPreFilterItem where
  readM = Brian.TrifectaPlus.readM

----------------------------------------

dateNDaysAgo ∷ MonadIO μ ⇒ℕ → μ 𝕋
dateNDaysAgo d = do
  today ← liftIO $ utctDay ⊳ getCurrentTime
  return $ T.pack ∘ iso8601Show $ addDays (-1*toInteger d) today

----------------------------------------

itemWhereClause ∷ MonadIO μ ⇒ DBEntryPreFilterItem → μ (𝕋,[SQLData])
itemWhereClause DBEntryNullFilter          = return ("TRUE", [])
itemWhereClause (DBEntryTitleFilter t)     = return ("title LIKE ?"  ,toRow [t])
itemWhereClause (DBEntryMediumFilter t)    = return ("medium LIKE ?", toRow [t])
itemWhereClause (DBEntryTagFilter g)       = return ("tag LIKE ?"    ,toRow [g])
itemWhereClause (DBEntryActressFilter a)   = return ("actress LIKE ?",toRow [a])
itemWhereClause (DBEntryEpNameFilter a)    =
  return ("episodename LIKE ?",toRow [a])
itemWhereClause (DBEntryEpIDFilter a)    =
  return ("episodeid LIKE ?",toRow [a])
itemWhereClause (DBEntryDescFilter t)      =
  return ("description LIKE ?",toRow [t])
itemWhereClause (DBEntryEntryDateFilter d) = do
  cutoff_date ← dateNDaysAgo d
  return ("EntryDate > ?", toRow [cutoff_date])

------------------------------------------------------------

data DBEntryPreFilter = DBPreF (PredicateFilter DBEntryPreFilterItem)
                      | DBPreNull

instance OptReader DBEntryPreFilter where
  readM = DBPreF ⊳ readM

----------------------------------------

null ∷ DBEntryPreFilter
null = DBPreNull

----------------------------------------

filter ∷ DBEntryPreFilterItem → DBEntryPreFilter
filter = DBPreF ∘ EF_Pred

----------------------------------------

filterNot ∷ DBEntryPreFilterItem → DBEntryPreFilter
filterNot = DBPreF ∘ EF_NotPred

----------------------------------------

-- if a string has no %, then break on words, and put % between each word and
-- before and after
mglob ∷ 𝕋 → 𝕋
mglob t =
  if "%" `T.isInfixOf` t then t else "%" ⊕ T.intercalate "%" (T.words t) ⊕ "%"

----------------------------------------

dateFilter ∷ ℕ → DBEntryPreFilter
dateFilter = filter ∘ DBEntryEntryDateFilter

----------------------------------------

descFilter ∷ 𝕋 → DBEntryPreFilter
descFilter = filter ∘ DBEntryDescFilter ∘ mglob

----------------------------------------

tagFilter ∷ 𝕋 → DBEntryPreFilter
tagFilter = filter ∘ DBEntryTagFilter

----------------------------------------

tagNotFilter ∷ 𝕋 → DBEntryPreFilter
tagNotFilter = filterNot ∘ DBEntryTagFilter

----------------------------------------

titleFilter ∷ 𝕋 → DBEntryPreFilter
titleFilter = filter ∘ DBEntryTitleFilter ∘ mglob

----------------------------------------

actressFilter ∷ 𝕋 → DBEntryPreFilter
actressFilter = filter ∘ DBEntryActressFilter ∘ mglob

----------------------------------------

whereClause ∷ MonadIO μ ⇒ DBEntryPreFilter → μ (𝕋,[SQLData])
whereClause DBPreNull = return ("TRUE",[])
whereClause (DBPreF (EF_Pred p))  = itemWhereClause p
whereClause (DBPreF (EF_NotPred p))  = (first ("NOT " ⊕)) ⊳ itemWhereClause p
whereClause (DBPreF (EF_Conj ps)) = do
  (clauses,datums) ← unzip ⊳ mapM whereClause (DBPreF ⊳ ps)
  return (parenthesize (T.intercalate " AND " $ toList clauses),
          ю (toList ⊳ datums))
whereClause (DBPreF (EF_Disj ps)) = do
  (clauses,datums) ← unzip ⊳ mapM whereClause (DBPreF ⊳ ps)
  return (parenthesize (T.intercalate " OR " $ toList clauses),
          ю (toList ⊳ datums))

----------------------------------------

gFilt ∷ DBEntryPreFilter
gFilt =
  disj (descFilter "%gag%")
       (conj (tagFilter "gagtype_%") (tagNotFilter "gagtype_hand%"))

----------------------------------------

conj ∷ DBEntryPreFilter → DBEntryPreFilter → DBEntryPreFilter
conj (DBPreF f) (DBPreF f') = DBPreF (PredicateFilter.conj f f')
conj (DBPreNull) d          = d
conj d (DBPreNull)          = d

----------------------------------------

disj ∷ DBEntryPreFilter → DBEntryPreFilter → DBEntryPreFilter
disj (DBPreF f) (DBPreF f') = DBPreF (PredicateFilter.disj f f')
disj (DBPreNull) d          = d
disj d (DBPreNull)          = d

-- tests -----------------------------------------------------------------------

{-| unit tests -}
parseTests ∷ TestTree
parseTests =
  testGroup "parseTest" $
    [ testParse "t{homeLand}"  (EF_Pred $ DBEntryTitleFilter   "%homeLand%")
    , testParse "a{Locklear}"  (EF_Pred $ DBEntryActressFilter "%Locklear%")
    , testParse "⋀[A{h},T{x}]" (EF_Conj ( EF_Pred (DBEntryActressFilter "h" ) :|
                                        [ EF_Pred $ DBEntryTitleFilter "x" ]))
    ]

{-| unit tests -}
tests ∷ TestTree
tests =
  testGroup "DBEntryPreFilter" [ parseTests ]

_test ∷ IO ExitCode
_test = runTestTree tests

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
