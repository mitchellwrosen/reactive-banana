{-# LANGUAGE RecordWildCards #-}

module Reactive.Banana.Latch
  ( Latch
  , Latch'(..)
  , seenL
  , valueL
  , readLatchB
  , readLatchP
  , readLatchFutureP
  , rememberLatchUpdate
  ) where

import Reactive.Banana.Action (Action(..))
import Reactive.Banana.Build (Build)
import Reactive.Banana.EvalL (EvalL)
import Reactive.Banana.EvalO (Future)
import Reactive.Banana.EvalP (EvalP)
import Reactive.Banana.Lens (Lens(..))
import Reactive.Banana.Ref (Ref, readRef)
import Reactive.Banana.Time (Time)

import qualified Reactive.Banana.EvalP as EvalP

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.ReaderWriterIO (runReaderWriterIOT)


type Latch a
  = Ref (Latch' a)

data Latch' a
  = Latch
  { _seenL  :: !Time               -- Timestamp for the current value.
  , _valueL :: a                   -- Current value.
  , _evalL  :: EvalL a             -- Recalculate current latch value.
  }

seenL :: Lens (Latch' a) Time
seenL = Lens _seenL  (\a s -> s { _seenL = a })

valueL :: Lens (Latch' a) a
valueL = Lens _valueL (\a s -> s { _valueL = a })

-- | Evaluate a latch (-computation) at the latest time,
-- but discard timestamp information.
readLatchIO :: Latch a -> IO a
readLatchIO latch = do
    Latch{..} <- readRef latch
    fst <$> runReaderWriterIOT _evalL ()

readLatchB :: Latch a -> Build a
readLatchB =
  liftIO . readLatchIO

readLatchP :: Latch a -> EvalP a
readLatchP =
  EvalP.liftBuildP . readLatchB

readLatchFutureP :: Latch a -> EvalP (Future a)
readLatchFutureP =
  return . readLatchIO

rememberLatchUpdate :: IO () -> EvalP ()
rememberLatchUpdate x =
  EvalP.tell ((Action x, mempty), mempty)
