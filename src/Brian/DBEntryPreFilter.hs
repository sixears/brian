{-# LANGUAGE UnicodeSyntax #-}
module Brian.DBEntryPreFilter
  ( DBEntryPreFilter
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

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.TrifectaPlus qualified

import Brian.PredicateFilter ( PredicateFilter(EF_Conj, EF_Disj, EF_Pred) )

--------------------------------------------------------------------------------

data DBEntryPreFilterItem = DBEntryTitleFilter 𝕋
                          | DBEntryEntryDateFilter ℕ
  deriving (Show)

--------------------

instance TextualPlus DBEntryPreFilterItem where
  textual' =
    let braced        = T.pack ⊳ boundedDoubledChars '{' '}'
        bracedGlobbed = surround "%" ⊳ braced
    in    char 't' ⋫ (DBEntryTitleFilter ⊳ bracedGlobbed)
        ∤ char 'T' ⋫ (DBEntryTitleFilter ⊳ braced)
        ∤ char 'd' ⋫ (DBEntryEntryDateFilter ⊳ parens (read ⊳ some digit))

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
itemWhereClause (DBEntryTitleFilter t)     = return ("title LIKE ?", toRow [t])
itemWhereClause (DBEntryEntryDateFilter d) = do
  cutoff_date ← dateNDaysAgo d
  return ("EntryDate > ?", toRow [cutoff_date])

------------------------------------------------------------

type DBEntryPreFilter = PredicateFilter DBEntryPreFilterItem

----------------------------------------

whereClause ∷ MonadIO μ ⇒ DBEntryPreFilter → μ (𝕋,[SQLData])
whereClause (EF_Pred p)  = itemWhereClause p
whereClause (EF_Conj ps) = do
  (clauses,datums) ← unzip ⊳ mapM whereClause ps
  return (parenthesize (T.intercalate " AND " $ toList clauses),
          ю (toList ⊳ datums))
whereClause (EF_Disj ps) = do
  (clauses,datums) ← unzip ⊳ mapM whereClause ps
  return (parenthesize (T.intercalate " OR " $ toList clauses),
          ю (toList ⊳ datums))

-- that's all, folks! ----------------------------------------------------------
