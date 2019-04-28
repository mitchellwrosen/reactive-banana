{-# LANGUAGE FlexibleInstances, RecordWildCards, TypeSynonymInstances #-}

module Reactive.Banana.Pulse
  ( Pulse
  , Pulse'(..)
  , childrenP
  , levelP
  , parentsP
  , seenP
  , readPulseP
  , writePulseP
  ) where

import {-# SOURCE #-} Reactive.Banana.EvalP (EvalP)
import Reactive.Banana.Lens (Lens(..))
import Reactive.Banana.Level (Level)
import Reactive.Banana.Ref (Ref, readRef)
import {-# SOURCE #-} Reactive.Banana.SomeNode (SomeNode)
import Reactive.Banana.Time (Time)

import {-# SOURCE #-} qualified Reactive.Banana.EvalP as EvalP

import Control.Monad
import Data.Hashable (hashWithSalt)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.Weak (Weak)

import qualified Data.Vault.Lazy as Vault


type Pulse a
  = Ref (Pulse' a)

data Pulse' a
  = Pulse
  { _keyP      :: Vault.Key (Maybe a) -- Key to retrieve pulse from cache.
  , _seenP     :: !Time               -- See note [Timestamp].
  , _evalP     :: EvalP (Maybe a)     -- Calculate current value.
  , _childrenP :: [Weak SomeNode]     -- Weak references to child nodes.
  , _parentsP  :: [Weak SomeNode]     -- Weak reference to parent nodes.
  , _levelP    :: !Level              -- Priority in evaluation order.
  , _nameP     :: String              -- Name for debugging.
  }

instance Show (Pulse a) where
  show p = _nameP (unsafePerformIO $ readRef p) ++ " " ++ show (hashWithSalt 0 p)

-- Lenses for various parameters
seenP :: Lens (Pulse' a) Time
seenP = Lens _seenP  (\a s -> s { _seenP = a })

parentsP :: Lens (Pulse' a) [Weak SomeNode]
parentsP = Lens _parentsP (\a s -> s { _parentsP = a })

childrenP :: Lens (Pulse' a) [Weak SomeNode]
childrenP = Lens _childrenP (\a s -> s { _childrenP = a })

levelP :: Lens (Pulse' a) Int
levelP = Lens _levelP (\a s -> s { _levelP = a })

readPulseP :: Pulse a -> EvalP (Maybe a)
readPulseP p = do
  Pulse{..} <- readRef p
  join . Vault.lookup _keyP <$> EvalP.get

writePulseP :: Vault.Key (Maybe a) -> Maybe a -> EvalP ()
writePulseP key a =
  EvalP.modify (Vault.insert key a)
