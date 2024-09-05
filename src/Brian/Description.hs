{-# LANGUAGE UnicodeSyntax #-}
module Brian.Description
  ( Description(Description)
  , more
  , unDescription
  ) where

import Base1T

-- base --------------------------------

import Data.List ( dropWhileEnd )
import GHC.Exts  ( IsString )

-- parsers -----------------------------

import Text.Parser.Char        ( noneOf )
import Text.Parser.Combinators ( (<?>) )

-- sqlite-simple -----------------------

import Database.SQLite.Simple.FromField ( FromField(fromField) )
import Database.SQLite.Simple.ToField   ( ToField(toField) )

-- text --------------------------------

import Data.Text ( intercalate, pack, unpack )

-- textual-plus ------------------------

import TextualPlus                         ( TextualPlus(textual'), parseText )
import TextualPlus.Error.TextualParseError ( tparseToME' )

--------------------------------------------------------------------------------

newtype Description = Description { unDescription :: 𝕋 }
  deriving newtype (Eq, IsString, Printable, Show)

instance TextualPlus Description where
  textual' = Description ∘ pack ⊳ (many (noneOf "\n")) <?> "Description"

instance ToField Description where
  toField = toField ∘ unDescription

instance FromField Description where
  fromField = Description ⩺ fromField

more ∷ Description → [𝕋] → Description
more (Description d) ts =
  Description (intercalate "\n" $ d:(dropWhileEnd (≡"") ts))

-- tests -----------------------------------------------------------------------

checkT ∷ (TextualPlus α, Eq α, Show α) ⇒ 𝕋 → α → TestTree
checkT input exp =
  testCase ("parseText: " ⊕ unpack input) $
    𝕽 exp @=? (tparseToME' ∘ parseText) input

{-| unit tests -}
tests ∷ TestTree
tests =
  testGroup "Description"
    [ checkT "foo"                       (Description "foo")
    , checkT "foo\nbar"                  (Description "foo\nbar")
    , checkT "foo\nbar\n"                (Description "foo\nbar\n")
    , checkT "foo\nbar\nbaz"             (Description "foo\nbar\nbaz")
    , checkT "foo\nbar\nbaz\nTags: quux" (Description "foo\nbar\nbaz")
    ]

_test ∷ IO ExitCode
_test = runTestTree tests

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
