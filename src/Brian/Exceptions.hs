{-# LANGUAGE UnicodeSyntax #-}
module Brian.Exceptions
  ( catch
  , catches
  ) where

import Base1T

-- safe-exceptions ---------------------

import Control.Exception qualified as Exception

--------------------------------------------------------------------------------

catch ∷ (MonadIO μ, Exception ε, MonadError α μ) ⇒ IO β → (ε → IO α) → μ β
catch io h =
  liftIO ((𝕽 ⊳ io) `Exception.catch` (𝕷 ⩺ h)) ≫ either throwError return

catches ∷ (MonadIO μ, MonadError α μ) ⇒ IO β → [Exception.Handler α] → μ β
catches io h = do
    liftIO ((𝕽 ⊳ io) `Exception.catches` (𝕷 ⊳⊳ h)) ≫ either throwError return

-- that's all, folks! ----------------------------------------------------------
