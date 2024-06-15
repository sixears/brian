{-# LANGUAGE UnicodeSyntax #-}
module Brian.TagSoup
  ( text
  , (≈)
  , (≉)
  ) where

import Base1T

-- tagsoup -----------------------------

import Text.HTML.TagSoup ( Tag, innerText, (~/=), (~==) )

-- text --------------------------------

import Data.Text ( unwords, words )

--------------------------------------------------------------------------------

(≈) ∷ Tag 𝕋 → 𝕊 → 𝔹
(≈) tag t = (~==) tag ("<" ⊕ t ⊕ ">")

(≉) ∷ Tag 𝕋 → 𝕊 → 𝔹
(≉) tag t = (~/=) tag ("<" ⊕ t ⊕ ">")

text ∷ [Tag 𝕋] → 𝕋
text = unwords ∘ words ∘ innerText

-- that's all, folks! ----------------------------------------------------------
