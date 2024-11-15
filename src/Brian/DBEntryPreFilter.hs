{-# LANGUAGE UnicodeSyntax #-}
module Brian.DBEntryPreFilter
  ( DBEntryPreFilter
  , DBEntryPreFilterItem(DBEntryEntryDateFilter)
  , gFilt
  , tests
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

import Brian.TrifectaPlus qualified

import Brian.PredicateFilter ( PredicateFilter(EF_Conj, EF_Disj, EF_None, EF_Pred) )

--------------------------------------------------------------------------------

data DBEntryPreFilterItem = DBEntryTitleFilter 𝕋
                          | DBEntryActressFilter 𝕋
                          | DBEntryDescFilter 𝕋
                          | DBEntryTagFilter 𝕋
                          | DBEntryEntryDateFilter ℕ
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
        ∤ char 'y' ⋫ (DBEntryEntryDateFilter ⊳ parens (read ⊳ some digit))
        ∤ char 'g' ⋫ (DBEntryTagFilter ⊳ bracedGlobbed)
        ∤ char 'G' ⋫ (DBEntryTagFilter ⊳ braced)

-- AND monoid
-- pre-G
-- medium,description,episodeid,episodename,entrydate

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
itemWhereClause (DBEntryTitleFilter t)     = return ("title LIKE ?"  ,toRow [t])
itemWhereClause (DBEntryTagFilter g)       = return ("tag LIKE ?"    ,toRow [g])
itemWhereClause (DBEntryActressFilter a)   = return ("actress LIKE ?",toRow [a])
itemWhereClause (DBEntryDescFilter t)      =
  return ("description LIKE ?",toRow [t])
itemWhereClause (DBEntryEntryDateFilter d) = do
  cutoff_date ← dateNDaysAgo d
  return ("EntryDate > ?", toRow [cutoff_date])

------------------------------------------------------------

type DBEntryPreFilter = PredicateFilter DBEntryPreFilterItem

----------------------------------------

whereClause ∷ MonadIO μ ⇒ DBEntryPreFilter → μ (𝕋,[SQLData])
whereClause EF_None      = return ("TRUE",[])
whereClause (EF_Pred p)  = itemWhereClause p
whereClause (EF_Conj ps) = do
  (clauses,datums) ← unzip ⊳ mapM whereClause ps
  return (parenthesize (T.intercalate " AND " $ toList clauses),
          ю (toList ⊳ datums))
whereClause (EF_Disj ps) = do
  (clauses,datums) ← unzip ⊳ mapM whereClause ps
  return (parenthesize (T.intercalate " OR " $ toList clauses),
          ю (toList ⊳ datums))

----------------------------------------

gFilt ∷ DBEntryPreFilter
gFilt =
  EF_Disj ( EF_Pred (DBEntryDescFilter "%gag%")
            :| [ -- no tags
  -- tag matching gagtype(!hand)
     ])

-- tests -----------------------------------------------------------------------

{-| unit tests -}
parseTests ∷ TestTree
parseTests =
  testGroup "parseTest" $
    [ testParse "t{homeLand}"  (EF_Pred $ DBEntryTitleFilter   "%homeLand%")
    , testParse "a{Locklear}"  (EF_Pred $ DBEntryActressFilter "%Locklear%")
    , testParse "⋀[A{h},T{x}]" (EF_Conj ( EF_Pred (DBEntryActressFilter "h" ) :|
                                        [ EF_Pred $ DBEntryTitleFilter "x" ]))
    {- , testParse "a{ Ha\\tcher}" (EF_Pred $ sef_actress_pcre [pcre| Ha\tcher|])
    , testParse "p(1.02.3)"     (EF_Pred $ sef_epid_match $ EpIDFilter [1,2,3])
    , testParse "e{bongi}"      (EF_Pred $ sef_epname_pcre $ [pcre|bongi|])
    , testParse "⋀[t{homeLand},p(04.05)]"
      (EF_Conj $ (EF_Pred $ sef_title_pcre [pcre|homeLand|])
              :| [EF_Pred ∘ sef_epid_match $ EpIDFilter [4,5]])
    , testParse "&& [ p(006)  ,t{homeLand} ]"
      (EF_Conj $ (EF_Pred $ sef_epid_match (EpIDFilter [6]))
              :| [EF_Pred $ sef_title_pcre [pcre|homeLand|]])
    , testParse "⋁[t{homeLand},p(04.05)]"
      (EF_Disj $ (EF_Pred $ sef_title_pcre [pcre|homeLand|])
              :| [EF_Pred ∘ sef_epid_match $ EpIDFilter [4,5]])
    , testParse "⋀[t{homeLand},⋁[p(04.05),  p(1.2)]]"
      (EF_Conj $ (EF_Pred $ sef_title_pcre [pcre|homeLand|])
              :| [EF_Disj $ (   EF_Pred ∘ sef_epid_match $ EpIDFilter [4,5])
                             :| [EF_Pred ∘ sef_epid_match $ EpIDFilter [1,2]]])
    -} ]

filtTests ∷ TestTree
filtTests =
  let {- flt_guiding = EF_Pred $ sef_title_pcre [pcre|Guiding|]
      flt_spider  = EF_Pred $ sef_title_pcre [pcre|Spider|]
      flt_ep1     = EF_Pred $ sef_epid_match (EpIDFilter [1])
      flt_ep2     = EF_Pred $ sef_epid_match (EpIDFilter [2])
      flt_spOR1   = EF_Disj (flt_spider :| [flt_ep1])
      flt_spAND1  = EF_Conj (flt_spider :| [flt_ep1]) -}

  in  testGroup "EntryFilter"
        [ {- testCase "Guiding:guiding +"$ matchFilt flt_guiding EntryData.e1 @=? 𝕿
        , testCase "Spider:guiding  -"$ matchFilt flt_guiding EntryData.e3 @=? 𝕱
        , testCase "Guiding:spider  -"$ matchFilt flt_spider  EntryData.e1 @=? 𝕱
        , testCase "Spider:spider   +"$ matchFilt flt_spider  EntryData.e3 @=? 𝕿
        , testCase "Guiding:1       -"$ matchFilt flt_ep1     EntryData.e1 @=? 𝕱
        , testCase "Spider:1        +"$ matchFilt flt_ep1     EntryData.e3 @=? 𝕿

        , testCase "Spider:⋀[spider,1] +"$
            matchFilt (EF_Conj (flt_spider :| [flt_ep1]))     EntryData.e3 @=? 𝕿
        , testCase "Spider:⋀[spider,2] +"$
            matchFilt (EF_Conj (flt_spider :| [flt_ep2]))     EntryData.e3 @=? 𝕱
        , testCase "Spider:⋀[guiding,1] +"$
            matchFilt (EF_Conj (flt_guiding :| [flt_ep1]))    EntryData.e3 @=? 𝕱
        , testCase "Spider:⋀[guiding,2] +"$
            matchFilt (EF_Conj (flt_guiding :| [flt_ep2]))    EntryData.e3 @=? 𝕱
        , testCase "Guiding:⋀[spider,1] +"$
            matchFilt (EF_Conj (flt_spider :| [flt_ep1]))     EntryData.e1 @=? 𝕱
        , testCase "Guiding:⋀[spider,2] +"$
            matchFilt (EF_Conj (flt_spider :| [flt_ep2]))     EntryData.e1 @=? 𝕱

        , testCase "Spider:⋁[spider,1] +"$
            matchFilt (EF_Disj (flt_spider :| [flt_ep1]))     EntryData.e3 @=? 𝕿
        , testCase "Spider:⋁[spider,2] +"$
            matchFilt (EF_Disj (flt_spider :| [flt_ep2]))     EntryData.e3 @=? 𝕿
        , testCase "Spider:⋁[guiding,1] +"$
            matchFilt (EF_Disj (flt_guiding :| [flt_ep1]))    EntryData.e3 @=? 𝕿
        , testCase "Spider:⋁[guiding,2] +"$
            matchFilt (EF_Disj (flt_guiding :| [flt_ep2]))    EntryData.e3 @=? 𝕱
        , testCase "Guiding:⋁[spider,1] +"$
            matchFilt (EF_Disj (flt_spider :| [flt_ep1]))     EntryData.e1 @=? 𝕱
        , testCase "Guiding:⋁[spider,2] +"$
            matchFilt (EF_Disj (flt_spider :| [flt_ep2]))     EntryData.e1 @=? 𝕱

        , testCase "Spider:⋀[guiding,⋁[spider,1]] +"$
            let filt = EF_Conj (flt_guiding :| [flt_spOR1])
            in  matchFilt filt EntryData.e3 @=? 𝕱
        , testCase "Spider:⋀[⋁[spider,1],guiding] +"$
            let filt = EF_Conj (flt_spOR1 :| [flt_guiding])
            in  matchFilt filt EntryData.e3 @=? 𝕱
        , testCase "Spider:⋁[⋀[spider,1],guiding] +"$
            let filt = EF_Disj (flt_spAND1 :| [flt_guiding])
            in  matchFilt filt EntryData.e3 @=? 𝕿
        -} ]

{-| unit tests -}
tests ∷ TestTree
tests =
  testGroup "DBEntryPreFilter" [ filtTests, parseTests ]

_test ∷ IO ExitCode
_test = runTestTree tests

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
