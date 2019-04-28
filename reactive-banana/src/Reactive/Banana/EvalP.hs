{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, RecordWildCards #-}

module Reactive.Banana.EvalP
  ( EvalP
  , runEvalP
  , wrapEvalP
  , unwrapEvalP
  , liftBuildP
  , askTime
  , get
  , modify
  , tell
  , rememberOutput
  ) where

import Reactive.Banana.Build (Build, BuildR, BuildW)
import Reactive.Banana.EvalO (EvalO, Future)
import Reactive.Banana.EvalPW (EvalPW)
import Reactive.Banana.Output (Output)
import Reactive.Banana.Pulse (Pulse, Pulse'(..))
import Reactive.Banana.Ref (readRef)
import Reactive.Banana.Time (Time)

import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.ReaderWriterIO (readerWriterIOT, runReaderWriterIOT)
import Control.Monad.Trans.RWSIO (RWSIOT, runRWSIOT, rwsT)
import Data.Vault.Lazy (Vault)

import qualified Control.Monad.Trans.RWSIO as RWS
import qualified Data.Vault.Lazy as Vault

-- Note: For efficiency reasons, we unroll the monad transformer stack.
-- type EvalP = RWST () Lazy.Vault EvalPW Build
newtype EvalP a
  = EvalP { unEvalP :: RWSIOT BuildR (EvalPW, BuildW) Vault IO a }
  deriving (Functor, Applicative, Monad, MonadIO)
    -- writer : (latch updates, IO action)
    -- state  : current pulse values

runEvalP :: Vault -> EvalP a -> Build (a, EvalPW)
runEvalP s1 m = readerWriterIOT $ \r2 -> do
  (a,_,(w1,w2)) <- runRWSIOT (unEvalP m) r2 s1
  return ((a,w1), w2)

liftBuildP :: Build a -> EvalP a
liftBuildP m = EvalP $ rwsT $ \r2 s -> do
  (a,w2) <- runReaderWriterIOT m r2
  return (a,s,(mempty,w2))

get :: EvalP Vault
get =
  EvalP RWS.get

modify :: (Vault -> Vault) -> EvalP ()
modify f =
  EvalP (RWS.modify f)

tell :: (EvalPW, BuildW) -> EvalP ()
tell w =
  EvalP (RWS.tell w)

askTime :: EvalP Time
askTime =
  EvalP (fst <$> RWS.ask)

rememberOutput :: (Output, EvalO) -> EvalP ()
rememberOutput x =
  EvalP (RWS.tell ((mempty,[x]),mempty))

-- worker wrapper to break sharing and support better inlining
unwrapEvalP ::
     RWS.Tuple BuildR (EvalPW, BuildW) Vault
  -> EvalP a
  -> IO a
unwrapEvalP r m = RWS.run (unEvalP m) r

wrapEvalP ::
     (RWS.Tuple BuildR (EvalPW, BuildW) Vault -> IO a)
  -> EvalP a
wrapEvalP m = EvalP (RWS.R m)
