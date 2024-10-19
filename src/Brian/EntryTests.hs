{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE UnicodeSyntax #-}
module Brian.EntryTests
  ( tests
  ) where

import Base1T hiding ( toList )

-- base --------------------------------

import Data.Maybe ( fromMaybe )
import GHC.Exts   ( toList )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit ( assertFailure )

-- tasty-plus --------------------------

import TastyPlus ( assertListEq )

-- text --------------------------------

import Data.Text qualified as T

-- textual-plus ------------------------

import TextualPlus                         ( parseText )
import TextualPlus.Error.TextualParseError ( tparseToME' )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Brian.EntryData qualified as EntryData

import Brian.Description ( Description(unDescription) )
import Brian.Entry       ( Entry, actresses, description, episode, medium,
                           recordNumber, tags, title )

--------------------------------------------------------------------------------

checkT ∷ 𝕋 → Entry → TestTree
checkT input exp =
  let tname = T.unpack ∘ fromMaybe "--XX--" ∘ head $ T.lines input in
  case (tparseToME' ∘ parseText) input of
    𝕷 e → testCase (tname ⊕ ": parseText") $ assertFailure $ show e
    𝕽 e →
        let tt ∷ ∀ α . (Eq α, Show α) ⇒ TestName → Lens' Entry α → TestTree
            tt nm ln = testCase nm $ exp ⊣ ln @=? e ⊣ ln
        in testGroup tname $
             [ tt "recordNumber" recordNumber
             , tt "title"        title
             , tt "medium"       medium
             , tt "actresses"    actresses
             , assertListEq "tags" (toList $ exp ⊣ tags) (toList $ e ⊣ tags)
             , tt "episode"      episode
             , assertListEq "description"
                            (T.lines ∘ unDescription $ exp ⊣ description)
                            (T.lines ∘ unDescription $ e ⊣ description)
             ]

{-| unit tests -}
tests ∷ TestTree
tests =
  testGroup "Entry" [ checkT t e | (t,e) ← EntryData.ets ]

_test ∷ IO ExitCode
_test = runTestTree tests

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
