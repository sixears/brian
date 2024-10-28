{-# LANGUAGE UnicodeSyntax #-}
module Brian.EntryFilter
  ( EntryFilter
    -- , entryMatches
  , gFilt
  , matchFilt
  ) where

import Base1T  hiding ( toList )
import Prelude ( error )

-- base --------------------------------

import Control.Monad.Fail ( MonadFail(fail) )
import Data.Char          ( isAlpha )
import Data.Foldable      ( all, and, any, or )
import Data.List          ( intercalate, repeat, zip )
import Data.Typeable      ( typeOf )
import GHC.Exts           ( toList )
import Text.Read          ( read )

-- lens --------------------------------

import Control.Lens.Getter ( view )

-- natural -----------------------------

import Natural ( length )

-- options-applicative -----------------

import Options.Applicative ( argument, eitherReader, help, metavar )

-- optparse-plus -----------------------

import OptParsePlus ( OptReader(readM) )

-- parsers -----------------------------

import Text.Parser.Char        ( CharParsing, char, digit, string )
import Text.Parser.Combinators ( sepBy, sepByNonEmpty, try )

-- parser-plus -------------------------

import ParserPlus ( boundedDoubledChars, brackets, parens, whitespaces )

-- pcre --------------------------------

import PCRE       ( PCRE, compRE, (?=~) )
import PCRE.Base  ( pcre, reSource )
import PCRE.Error ( REParseError )

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

import Text.Trifecta.Result ( Result(Failure, Success) )

-- trifecta-plus -----------------------

import TrifectaPlus ( eiText, tParse, testParse )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.EntryData qualified as EntryData

import Brian.BTag        ( unBTags )
import Brian.Description ( Description )
import Brian.Entry       ( Entry, actresses, description, episode, tags, title )
import Brian.Episode     ( EpisodeID(unEpisodeID), epID, epName )
import Brian.OptParser   ( OptParser(optParse) )

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

{-
instance OptParser EntryFilter where
  optParse = EntryFilter ⊳ many (option readM (ю [ long "title"
                                                 , short 't'
                                                 , help "title match PCRE"
                                                 ]))
                         ⊵ many (strOption (ю [ long "title-filter"
                                                 , short 'T'
                                                 , help "title LIKE filter"
                                                 ]))
                         ⊵ many (option readM (ю [ long "actress"
                                                 , short 'a'
                                                 , help "actress match PCRE"
                                                 ]))
                         ⊵ many (option readM (ю [ long "description"
                                                 , short 'd'
                                                 , help "description match PCRE"
                                                 ]))
                         ⊵ many (option readM (ю [ long "epname"
                                                 , long "episode-name"
                                                 , short 'e'
                                                 , help "episode name PCRE"
                                                 ]))
                         ⊵ many (option readM (ю [ long "epid"
                                                 , long "episode-id"
                                                 , short 'p'
                                                 , help "episode ID"
                                                 ]))
-}

----------------------------------------

gFilt ∷ Entry → 𝔹
gFilt e =
  let words ∷ 𝕋 → [𝕋] = T.split (ﬧ . isAlpha)
      paired_words ∷ 𝕋 → [(𝕋,𝕋)] = (\ xs → zip xs (tailSafe xs)) ∘ words
      descn_filter ∷ Description → 𝔹 =
        let f ∷ (𝕋,𝕋) → 𝔹
              = \ (a,b) → "gag" `T.isPrefixOf` (T.toLower b)
                        ∧ (T.toLower a) ∉ ["no", "not"]
        in  any f ∘ paired_words ∘ toText
      tag_filter   = [pcre|^gagtype_(?!hand)|]    -- negative lookahead
  in  or [ descn_filter (e ⊣ description)
         , any (\ t → matched $ t ?=~ tag_filter) $ toText ⩺ unBTags $ e ⊣ tags
         ]

------------------------------------------------------------

data EntryFilter = EF_Conj (NonEmpty EntryFilter)
                 | EF_Disj (NonEmpty EntryFilter)
                 | EF_Pred 𝕋 (Entry -> 𝔹)

--------------------

instance Show EntryFilter where
  show (EF_Pred t _) = T.unpack t
  show (EF_Conj xs)  = "AND[" ⊕ intercalate "," (show ⊳ toList xs) ⊕ "]"
  show (EF_Disj xs)  = "OR[" ⊕ intercalate "," (show ⊳ toList xs) ⊕ "]"

--------------------

instance Eq EntryFilter where
  EF_Pred t _ == EF_Pred t' _ = t == t'
  EF_Conj xs  == EF_Conj xs'  = and $
    (length xs ≡ length xs'): [ x ≡ x' | (x,x') ← zip (toList xs) (toList xs') ]
  EF_Disj xs  == EF_Disj xs'  = and $
    (length xs ≡ length xs'): [ x ≡ x' | (x,x') ← zip (toList xs) (toList xs') ]
  _           == _            = 𝕱

----------------------------------------

parseRE ∷ (MonadFail μ, CharParsing μ) ⇒ μ PCRE
parseRE =
  eitherParsec (boundedDoubledChars '{' '}') (compRE @REParseError ∘ T.pack)

----------------------------------------

parseEPID ∷ (MonadFail μ, CharParsing μ) ⇒ μ EpIDFilter
parseEPID = parens textual'

----------------------------------------

parseFilts ∷ (MonadFail μ, CharParsing μ) ⇒ μ (NonEmpty EntryFilter)
parseFilts =
  let whitespaced p = whitespaces ⋫ p ⋪ whitespaces
      separator     = whitespaced $ char ','
  in  brackets (whitespaced (textual' `sepByNonEmpty` try (separator)))

----------------------------------------

ef2_title_pcre ∷ PCRE → EntryFilter
ef2_title_pcre re   =
  EF_Pred ([fmt|title PCRE: %s|] (reSource re))
          (\ e → matched $ toText(e ⊣ title) ?=~ re)

----------------------------------------

ef2_actress_pcre ∷ PCRE → EntryFilter
ef2_actress_pcre re   =
  EF_Pred ([fmt|actress PCRE: %s|] (reSource re))
          (\ e → or ((matched ∘ (?=~re) ∘ toText) ⊳ toList (e ⊣ actresses)))

----------------------------------------

ef2_epid_match ∷ EpIDFilter → EntryFilter
ef2_epid_match epidf =
  EF_Pred ([fmt|epID: %T|] epidf)
          (\ e → maybe 𝕱 (matchEpID epidf) (view epID ⊳ e ⊣ episode))

----------------------------------------

ef2_epname_pcre ∷ PCRE → EntryFilter
ef2_epname_pcre re   =
  EF_Pred ([fmt|epname PCRE: %s|] (reSource re))
          (\ e → maybe 𝕱 (\ n → matched $ toText n ?=~ re)
                         (e ⊣ episode ≫ view epName))

----------------------------------------

instance TextualPlus EntryFilter where
  textual' = char 'p' ⋫ (ef2_epid_match ⊳ parseEPID)
             -- The TextualPlus instance of PCRE allows for double-quoting.
             -- I guess that was a mistake; but anyway, we cannot easily use it
             -- here directly without adding complication to the parsing (for
             -- users)
           ∤ char 't' ⋫ (ef2_title_pcre ⊳ parseRE)
           ∤ char 'a' ⋫ (ef2_actress_pcre ⊳ parseRE)
           ∤ char 'e' ⋫ (ef2_epname_pcre ⊳ parseRE)
           ∤ ((string "⋀" ∤ string "&&") ⋪ whitespaces) ⋫ (EF_Conj ⊳ (whitespaces ⋫ parseFilts))
           ∤ (string "⋁" ∤ string "||") ⋫ (EF_Disj ⊳ parseFilts)

----------------------------------------

trifectTextual ∷ ∀ β α η .
                 (TextualPlus β, Printable α, Typeable β, MonadError 𝕋 η) ⇒
                 α → η β
trifectTextual (toText → z) =
  let fromParsed (Success a) = a
      -- this function exists solely to provide a hypothetical value to reflect
      -- on
      fromParsed (Failure _) = error "this should never be evaluated"
      parsedZ                = tParse z
      typ                    = typeOf $ fromParsed parsedZ
   in case parsedZ of
        Success a → return a
        Failure e →
          throwError $ [fmt|failed to parse '%t' as '%w': %t|] z typ (eiText e)

instance OptParser EntryFilter where
  optParse = argument (eitherReader (first T.unpack ⊳ trifectTextual))
                      (metavar "PREDICATE" ⊕ help "episode filter")

----------------------------------------

{- | Take a parsec for an α, and function of the form `α → Either Printable β`,
     and use these to build a `ParsecT`.
 -}
eitherParsec ∷ (MonadFail μ, CharParsing μ, Printable ε) ⇒
               μ α → (α → 𝔼 ε β) → μ β
eitherParsec f g = f ≫ (\ t → case g t of
                                 𝕷 e → fail $ toString e
                                 𝕽 r → return r)

----------------------------------------

matchFilt ∷ EntryFilter → Entry → 𝔹
matchFilt (EF_Pred _ p)  e = p e
matchFilt (EF_Conj ps) e   = all (\ p -> matchFilt p e) ps
matchFilt (EF_Disj ps) e   = any (\ p -> matchFilt p e) ps

--------------------------------------------------------------------------------

{-| unit tests -}
parseTests ∷ TestTree
parseTests =
  testGroup "parseTest" $
    [ testParse "t{homeLand}" (ef2_title_pcre [pcre|homeLand|])
    , testParse "a{ Ha\\tcher}" (ef2_actress_pcre [pcre| Ha\tcher|])
    , testParse "p(1.02.3)" (ef2_epid_match $ EpIDFilter [1,2,3])
    , testParse "e{bongi}" (ef2_epname_pcre $ [pcre|bongi|])
    , testParse "⋀[t{homeLand},p(04.05)]"
      (EF_Conj $ ef2_title_pcre [pcre|homeLand|]
              :| [ef2_epid_match $ EpIDFilter [4,5]])
    , testParse "&& [ p(006)  ,t{homeLand} ]"
      (EF_Conj $ ef2_epid_match (EpIDFilter [6])
              :| [ef2_title_pcre [pcre|homeLand|]])
    , testParse "⋁[t{homeLand},p(04.05)]"
      (EF_Disj $ ef2_title_pcre [pcre|homeLand|]
              :| [ef2_epid_match $ EpIDFilter [4,5]])
    , testParse "⋀[t{homeLand},⋁[p(04.05),  p(1.2)]]"
      (EF_Conj $ ef2_title_pcre [pcre|homeLand|]
              :| [EF_Disj $ (ef2_epid_match $ EpIDFilter [4,5]) :| [ef2_epid_match $ EpIDFilter [1,2]]])
    ]

filtTests ∷ TestTree
filtTests =
  let flt_guiding = ef2_title_pcre [pcre|Guiding|]
      flt_spider  = ef2_title_pcre [pcre|Spider|]
      flt_ep1     = ef2_epid_match (EpIDFilter [1])
      flt_ep2     = ef2_epid_match (EpIDFilter [2])
      flt_spOR1   = EF_Disj (flt_spider :| [flt_ep1])
      flt_spAND1  = EF_Conj (flt_spider :| [flt_ep1])

  in  testGroup "EntryFilter"
        [ testCase "Guiding:guiding +"$ matchFilt flt_guiding EntryData.e1 @=? 𝕿
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
