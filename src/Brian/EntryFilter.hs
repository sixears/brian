{-# LANGUAGE UnicodeSyntax #-}
module Brian.EntryFilter
  ( EntryFilter
  , gFilt
  , matchFilt
  , tests
  ) where

import Base1T hiding ( toList )

-- base --------------------------------

import Control.Monad.Fail ( MonadFail(fail) )
import Data.Char          ( isAlpha )
import Data.Foldable      ( and, any, or )
import Data.List          ( repeat, zip )
import GHC.Exts           ( toList )
import Text.Read          ( read )

-- lens --------------------------------

import Control.Lens.Getter ( view )

-- options-applicative -----------------

import Options.Applicative ( eitherReader, helpDoc, metavar, value )

-- optparse-plus -----------------------

import OptParsePlus ( OptReader(readM) )

-- parsers -----------------------------

import Text.Parser.Char        ( CharParsing, char, digit )
import Text.Parser.Combinators ( sepBy )

-- parser-plus -------------------------

import ParserPlus ( boundedDoubledChars, parens )

-- pcre --------------------------------

import PCRE       ( PCRE, compRE, (?=~) )
import PCRE.Base  ( pcre, reSource )
import PCRE.Error ( REParseError )

-- prettyprinter -----------------------

import Prettyprinter ( Doc, align, hsep, indent, pretty, vsep )

-- regex -------------------------------

import Text.RE.TDFA ( matched )

-- regex-with-pcre ---------------------

import Text.RE.PCRE.Text ()

-- safe --------------------------------

import Safe ( tailSafe )

-- text --------------------------------

import Data.Text qualified as T

-- text-printer ------------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus ( TextualPlus(textual'), parseTextual )

-- trifecta ----------------------------

import Text.Trifecta ( Parser )

-- trifecta-plus -----------------------

import TrifectaPlus ( testParse )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.EntryData       qualified as EntryData
import Brian.PredicateFilter qualified as PredicateFilter

import Brian.BTag            ( unBTags )
import Brian.Description     ( Description )
import Brian.Entry           ( Entry, actresses, description, episode, tags,
                               title )
import Brian.Episode         ( EpisodeID(unEpisodeID), epID, epName )
import Brian.OptParser       ( OptMkParser(optMkParse) )
import Brian.PredicateFilter ( PredicateFilter(EF_Conj, EF_Disj, EF_Pred),
                               ShowablePredicate(predMatch) )

--------------------------------------------------------------------------------

newtype EpIDFilter = EpIDFilter { unEpIDFilter :: [ℕ] }
  deriving (Eq)

instance Printable EpIDFilter where
  print = P.text ∘ T.intercalate "." ∘ fmap (T.pack ∘ show) ∘ unEpIDFilter

instance TextualPlus EpIDFilter where
  textual' = EpIDFilter ⊳ (read ⊳ some digit) `sepBy` char '.'

instance OptReader EpIDFilter where
  readM = eitherReader $ parseTextual

matchEpID ∷ EpIDFilter → EpisodeID → 𝔹
matchEpID (EpIDFilter fs) (unEpisodeID → ds) =
   and [ maybe 𝕱 (≡ f) d  | (f,d) ← zip fs ((𝕵 ⊳ ds) ⊕ repeat 𝕹) ]

------------------------------------------------------------

gFilt ∷ Entry → 𝔹
gFilt e =
  let words ∷ 𝕋 → [𝕋] = T.split (ﬧ . isAlpha)
      paired_words ∷ 𝕋 → [(𝕋,𝕋)] = (\ xs → zip xs (tailSafe xs)) ∘ words
      descn_filter ∷ Description → 𝔹 =
        let f ∷ (𝕋,𝕋) → 𝔹
              = \ (a,b) → "gag" `T.isInfixOf` (T.toLower b)
                        ∧ (T.toLower a) ∉ ["no", "not"]
        in  any f ∘ paired_words ∘ toText
      tag_filter = [pcre|^gagtype_(?!hand)|]    -- negative lookahead
      etags = toText ⩺ unBTags $ e ⊣ tags
  in  or [ descn_filter (e ⊣ description)
         , etags ≡ [] ∨ any (\ t → matched $ t ?=~ tag_filter) etags
         ]

------------------------------------------------------------

{-| Filter on α, with ability to construct arbitrary conjunctions & disjunctions
    of many filters.

    The base predicate comes with a 𝕋, which is used both for the `Show`
    instance and for equality.  Use with care, make sure that t the 𝕋 does
    indeed indicate equality.
-}

data ShowableEntryFilter = ShowableEntryFilter 𝕋 (Entry -> 𝔹)

instance Show ShowableEntryFilter where
  show (ShowableEntryFilter t _) = T.unpack t

instance Eq ShowableEntryFilter where
  (ShowableEntryFilter t _) == (ShowableEntryFilter t' _) = t ≡ t'

instance ShowablePredicate ShowableEntryFilter Entry where
  predMatch (ShowableEntryFilter _ p) = p

------------------------------------------------------------

data EntryFilter = EFSome (PredicateFilter ShowableEntryFilter)
                 | EFNone

----------------------------------------

-- instance OptReader EntryFilter where
--  readM = PredicateFilter.readM

instance OptMkParser EntryFilter where
{-
  optParse = let help_doc = helpDoc ∘ 𝕵 $ textualHelpDoc
             in  argument (EFSome ⊳ readM) (metavar "PREDICATE" ⊕ help_doc ⊕ value null)
-}
  optMkParse f m = let help_doc = helpDoc ∘ 𝕵 $ textualHelpDoc
                   in  f (EFSome ⊳ readM) (ю [ metavar "PREDICATE", help_doc
                                             , value null, m ])

----------------------------------------

null ∷ EntryFilter
null = EFNone

----------------------------------------

conj ∷ EntryFilter → EntryFilter → EntryFilter
conj EFNone e              = e
conj e EFNone              = e
conj (EFSome e) (EFSome ē) = EFSome (PredicateFilter.conj e ē)

----------------------------------------

disj ∷ EntryFilter → EntryFilter → EntryFilter
disj EFNone e              = e
disj e EFNone              = e
disj (EFSome e) (EFSome ē) = EFSome (PredicateFilter.disj e ē)

------------------------------------------------------------

{- | Take a parsec for an α, and function of the form `α → Either Printable β`,
     and use these to build a `ParsecT`.
 -}
eitherParsec ∷ (MonadFail μ, CharParsing μ, Printable ε) ⇒
               μ α → (α → 𝔼 ε β) → μ β
eitherParsec f g = f ≫ (\ t → case g t of
                                 𝕷 e → fail $ toString e
                                 𝕽 r → return r)

----------------------------------------

parseRE ∷ (MonadFail μ, CharParsing μ) ⇒ μ PCRE
parseRE =
  eitherParsec (boundedDoubledChars '{' '}') (compRE @REParseError ∘ T.pack)

----------------------------------------

parseEPID ∷ (MonadFail μ, CharParsing μ) ⇒ μ EpIDFilter
parseEPID = parens textual'

----------------------------------------

sef_title_pcre ∷ PCRE → ShowableEntryFilter
sef_title_pcre re   =
  ShowableEntryFilter ([fmt|title PCRE: %s|] (reSource re))
                      (\ e → matched $ toText(e ⊣ title) ?=~ re)

----------------------------------------

sef_actress_pcre ∷ PCRE → ShowableEntryFilter
sef_actress_pcre re   =
  ShowableEntryFilter ([fmt|actress PCRE: %s|] (reSource re))
                      (\ e → or ((matched ∘ (?=~re) ∘ toText) ⊳
                                  toList (e ⊣ actresses)))

----------------------------------------

sef_epid_match ∷ EpIDFilter → ShowableEntryFilter
sef_epid_match epidf =
  ShowableEntryFilter ([fmt|epID: %T|] epidf)
                      (\ e → maybe 𝕱 (matchEpID epidf)(view epID ⊳ e ⊣ episode))

----------------------------------------

sef_epname_pcre ∷ PCRE → ShowableEntryFilter
sef_epname_pcre re   =
  ShowableEntryFilter ([fmt|epname PCRE: %s|] (reSource re))
                      (\ e → maybe 𝕱 (\ n → matched $ toText n ?=~ re)
                                     (e ⊣ episode ≫ view epName))

----------------------------------------

data ParseType = ParseEPID | ParseRE

parseSpecs ∷ (CharParsing η, MonadFail η) ⇒
             [(ℂ, η ShowableEntryFilter, ParseType, 𝕋, 𝕋)]
parseSpecs =
  let parse_re   c p t =
        (c, p ⊳ parseRE  , ParseRE  , c `T.cons` "{..}", t ⊕ " PCRE")
      parse_epid c p t =
        (c, p ⊳ parseEPID, ParseEPID, c `T.cons` "(..)", t ⊕ " (m.n..)")
  in  [ parse_epid 'p' sef_epid_match "filter Episode ID (m.n..)"
      , parse_re 't' sef_title_pcre "filter Title"
      , parse_re 'a' sef_actress_pcre "filter Actress"
      , parse_re 'e' sef_epname_pcre "filter Episode Name"
      ]

parseSpecDescs ∷ [(Doc α,Doc α)]
parseSpecDescs =
  [ (pretty x, pretty t) | (_,_,_,x,t) ← parseSpecs @Parser]

instance TextualPlus ShowableEntryFilter where
  textual' = foldr1 (∤) [ char c ⋫ p | (c,p,_,_,_) ← parseSpecs ]

textualHelpDoc ∷ Doc α
textualHelpDoc =
  let columns = vsep [ c ⊕ (indent 4 t) | (c,t) ← parseSpecDescs ]
  in  vsep [ hsep [ "entry filter:", PredicateFilter.textualHelpDoc ]
           , indent 2 ∘ align $ columns ]

matchFilt ∷ EntryFilter → Entry → 𝔹
matchFilt (EFSome f) = PredicateFilter.matchFilt f
matchFilt EFNone     = const 𝕿

-- tests -----------------------------------------------------------------------

{-| unit tests -}
parseTests ∷ TestTree
parseTests =
  testGroup "parseTest" $
    [ testParse "t{homeLand}"   (EF_Pred $ sef_title_pcre [pcre|homeLand|])
    , testParse "a{ Ha\\tcher}" (EF_Pred $ sef_actress_pcre [pcre| Ha\tcher|])
    , testParse "p(1.02.3)"     (EF_Pred $ sef_epid_match $ EpIDFilter [1,2,3])
    , testParse "e{bongi}"      (EF_Pred $ sef_epname_pcre $ [pcre|bongi|])
    , testParse "⋀[t{homeLand},p(04.05)]"
      (EF_Conj $ (EF_Pred $ sef_title_pcre [pcre|homeLand|])
              :| [EF_Pred ∘ sef_epid_match $ EpIDFilter [4,5]])
    , testParse "AND [ p(006)  ,t{homeLand} ]"
      (EF_Conj $ (EF_Pred $ sef_epid_match (EpIDFilter [6]))
              :| [EF_Pred $ sef_title_pcre [pcre|homeLand|]])
    , testParse "⋁[t{homeLand},p(04.05)]"
      (EF_Disj $ (EF_Pred $ sef_title_pcre [pcre|homeLand|])
              :| [EF_Pred ∘ sef_epid_match $ EpIDFilter [4,5]])
    , testParse "⋀[t{homeLand},⋁[p(04.05),  p(1.2)]]"
      (EF_Conj $ (EF_Pred $ sef_title_pcre [pcre|homeLand|])
              :| [EF_Disj $ (   EF_Pred ∘ sef_epid_match $ EpIDFilter [4,5])
                             :| [EF_Pred ∘ sef_epid_match $ EpIDFilter [1,2]]])
    ]

filtTests ∷ TestTree
filtTests =
  let flt_guiding = EFSome ∘ EF_Pred $ sef_title_pcre [pcre|Guiding|]
      flt_spider  = EFSome ∘ EF_Pred $ sef_title_pcre [pcre|Spider|]
      flt_ep1     = EFSome ∘ EF_Pred $ sef_epid_match (EpIDFilter [1])
      flt_ep2     = EFSome ∘ EF_Pred $ sef_epid_match (EpIDFilter [2])
      flt_spOR1   = disj flt_spider flt_ep1
      flt_spAND1  = conj flt_spider flt_ep1

  in  testGroup "EntryFilter"
        [ testCase "Guiding:guiding +"$ matchFilt flt_guiding EntryData.e1 @=? 𝕿
        , testCase "Spider:guiding  -"$ matchFilt flt_guiding EntryData.e3 @=? 𝕱
        , testCase "Guiding:spider  -"$ matchFilt flt_spider  EntryData.e1 @=? 𝕱
        , testCase "Spider:spider   +"$ matchFilt flt_spider  EntryData.e3 @=? 𝕿
        , testCase "Guiding:1       -"$ matchFilt flt_ep1     EntryData.e1 @=? 𝕱
        , testCase "Spider:1        +"$ matchFilt flt_ep1     EntryData.e3 @=? 𝕿

        , testCase "Spider:⋀[spider,1] +"$
            matchFilt (conj flt_spider flt_ep1)     EntryData.e3 @=? 𝕿
        , testCase "Spider:⋀[spider,2] +"$
            matchFilt (conj flt_spider flt_ep2)     EntryData.e3 @=? 𝕱
        , testCase "Spider:⋀[guiding,1] +"$
            matchFilt (conj flt_guiding flt_ep1)    EntryData.e3 @=? 𝕱
        , testCase "Spider:⋀[guiding,2] +"$
            matchFilt (conj flt_guiding flt_ep2)    EntryData.e3 @=? 𝕱
        , testCase "Guiding:⋀[spider,1] +"$
            matchFilt (conj flt_spider flt_ep1)     EntryData.e1 @=? 𝕱
        , testCase "Guiding:⋀[spider,2] +"$
            matchFilt (conj flt_spider flt_ep2)     EntryData.e1 @=? 𝕱

        , testCase "Spider:⋁[spider,1] +"$
            matchFilt (disj flt_spider flt_ep1)     EntryData.e3 @=? 𝕿
        , testCase "Spider:⋁[spider,2] +"$
            matchFilt (disj flt_spider flt_ep2)     EntryData.e3 @=? 𝕿
        , testCase "Spider:⋁[guiding,1] +"$
            matchFilt (disj flt_guiding flt_ep1)    EntryData.e3 @=? 𝕿
        , testCase "Spider:⋁[guiding,2] +"$
            matchFilt (disj flt_guiding flt_ep2)    EntryData.e3 @=? 𝕱
        , testCase "Guiding:⋁[spider,1] +"$
            matchFilt (disj flt_spider flt_ep1)     EntryData.e1 @=? 𝕱
        , testCase "Guiding:⋁[spider,2] +"$
            matchFilt (disj flt_spider flt_ep2)     EntryData.e1 @=? 𝕱

        , testCase "Spider:⋀[guiding,⋁[spider,1]] +"$
            let filt = conj flt_guiding flt_spOR1
            in  matchFilt filt EntryData.e3 @=? 𝕱
        , testCase "Spider:⋀[⋁[spider,1],guiding] +"$
            let filt = conj flt_spOR1 flt_guiding
            in  matchFilt filt EntryData.e3 @=? 𝕱
        , testCase "Spider:⋁[⋀[spider,1],guiding] +"$
            let filt = disj flt_spAND1 flt_guiding
            in  matchFilt filt EntryData.e3 @=? 𝕿
        ]

{-| unit tests -}
tests ∷ TestTree
tests =
  testGroup "EntryFilter" [ filtTests, parseTests ]

_test ∷ IO ExitCode
_test = runTestTree tests

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
