module Data.Abc.PSoM.Types (PSoMBar) where

import Data.Maybe (Maybe)
import Data.Abc (Volta)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Data.Abc.PSoM (PSMusic)

-- | a bar of PSOM music
type PSoMBar =
  { number :: Int                             -- sequential from zero
  , endRepeats :: Int                         -- an end repeat (n >= 0)
  , startRepeats :: Int                       -- a start repeat (n >= 0)
  , iteration :: Maybe (NonEmptyList Volta)   -- an iteration volta marker  (|1  or |2 or |1-3 etc)
  , psomMessages :: List PSMusic              -- the notes in the bar
  }