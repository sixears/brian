{-# LANGUAGE UnicodeSyntax #-}
module Brian.Day
  ( Day(Day)
  ) where

import Base1T
import Debug.Trace ( traceShow )

-- base --------------------------------

import Control.Monad.Fail ( MonadFail )
import Data.Maybe         ( fromMaybe )

-- mtl ---------------------------------

import Control.Monad.Reader ( ask, runReaderT )

-- options-applicative -----------------

import Options.Applicative ( eitherReader )

-- optparse-plus -----------------------

import OptParsePlus ( OptReader(readM) )

-- sqlite-simple -----------------------

import Database.SQLite.Simple.FromField ( FromField(fromField), fieldData )
import Database.SQLite.Simple.ToField   ( ToField(toField) )

-- text-printer ------------------------

import Text.Printer qualified as P

-- time --------------------------------

import Data.Time.Calendar.OrdinalDate qualified as OrdinalDate

import Data.Time.Calendar.OrdinalDate ( fromOrdinalDate )
import Data.Time.Format.ISO8601       ( FormatExtension(ExtendedFormat),
                                        ISO8601(iso8601Format), calendarFormat,
                                        iso8601ParseM, iso8601Show )

--------------------------------------------------------------------------------

newtype Day = Day { unDay :: OrdinalDate.Day }
  deriving (Eq, Show)

parseISO8601 ∷ MonadFail η ⇒ 𝕊 → η Day
parseISO8601 = runReaderT $ ask ≫ Day ⩺ iso8601ParseM

instance Printable Day where
  print = P.string ∘ iso8601Show ∘ unDay

instance FromField Day where
  fromField f =
    maybe (Day $ fromOrdinalDate 1973 1) Day ∘ iso8601ParseM ⊳ fromField f

instance ToField Day where
  toField = toField ∘ iso8601Show ∘ unDay

instance OptReader Day where
  readM =
    let noparse s = [fmt|failed to parse '%s' as Day|] s
    in  eitherReader $ \ s → maybe (𝕷 $ noparse s) 𝕽 $ parseISO8601 s

-- that's all, folks! ----------------------------------------------------------
