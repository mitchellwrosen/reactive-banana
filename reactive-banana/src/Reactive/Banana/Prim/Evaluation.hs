{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecordWildCards #-}
module Reactive.Banana.Prim.Evaluation (
    step
    ) where

import Reactive.Banana.Action (doit)
import Reactive.Banana.Build (BuildR, BuildW)
import Reactive.Banana.EvalO (EvalO)
import Reactive.Banana.EvalP (EvalP, askTime, rememberOutput, runEvalP,
                              unwrapEvalP, wrapEvalP)
import Reactive.Banana.EvalPW (EvalPW)
import Reactive.Banana.Latch (Latch'(..), rememberLatchUpdate)
import Reactive.Banana.LatchWrite (LatchWrite'(..))
import Reactive.Banana.Level (Level, ground)
import Reactive.Banana.Network (Network(..))
import Reactive.Banana.Output (Output, Output'(..))
import Reactive.Banana.Prim.Plumbing
import Reactive.Banana.Prim.Types
import Reactive.Banana.Prim.Util
import Reactive.Banana.Pulse (Pulse'(..), readPulseP, writePulseP)
import Reactive.Banana.Ref (modify', put, readRef)
import Reactive.Banana.SomeNode (SomeNode(..))
import Reactive.Banana.Time (next)

import qualified Reactive.Banana.Prim.OrderedBag as OB

import Control.Monad (foldM)
import Control.Monad (join)
import Control.Monad.IO.Class
import Data.Functor
import Data.Maybe
import System.Mem.Weak

import qualified Control.Exception as Strict (evaluate)
import qualified Control.Monad.Trans.ReaderWriterIO as RW
import qualified Control.Monad.Trans.RWSIO as RWS
import qualified Data.PQueue.Prio.Min as Q
import qualified Data.Vault.Lazy as Lazy

type Queue = Q.MinPQueue Level

{-----------------------------------------------------------------------------
    Evaluation step
------------------------------------------------------------------------------}
-- | Evaluate all the pulses in the graph,
-- Rebuild the graph as necessary and update the latch values.
step :: ([SomeNode], Lazy.Vault) -> Network -> IO (IO (), Network)
step (inputs,pulses)
        Network{ nTime = time1
        , nOutputs = outputs1
        , nAlwaysP = Just alwaysP   -- we assume that this has been built already
        }
    = do

    -- evaluate pulses
    ((_, (latchUpdates, outputs)), topologyUpdates, os)
            <- runBuildIO (time1, alwaysP)
            $  runEvalP pulses
            $  evaluatePulses inputs

    doit latchUpdates                           -- update latch values from pulses
    doit topologyUpdates                        -- rearrange graph topology
    let actions :: [(Output, EvalO)]
        actions = OB.inOrder outputs outputs1   -- EvalO actions in proper order

        state2 :: Network
        state2  = Network
            { nTime    = next time1
            , nOutputs = OB.inserts outputs1 os
            , nAlwaysP = Just alwaysP
            }
    return (runEvalOs $ map snd actions, state2)

runEvalOs :: [EvalO] -> IO ()
runEvalOs = sequence_ . map join

{-----------------------------------------------------------------------------
    Traversal in dependency order
------------------------------------------------------------------------------}
-- | Update all pulses in the graph, starting from a given set of nodes
evaluatePulses :: [SomeNode] -> EvalP ()
evaluatePulses roots = wrapEvalP $ \r -> go r =<< insertNodes r roots Q.empty
    where
    go :: RWS.Tuple BuildR (EvalPW, BuildW) Lazy.Vault -> Queue SomeNode -> IO ()
    go r q =
        case Q.minView q of
            Nothing         -> return ()
            Just (node, q)  -> do
                children <- unwrapEvalP r (evaluateNode node)
                q        <- insertNodes r children q
                go r q

-- | Recalculate a given node and return all children nodes
-- that need to evaluated subsequently.
evaluateNode :: SomeNode -> EvalP [SomeNode]
evaluateNode (P p) = do
    Pulse{..} <- readRef p
    ma        <- _evalP
    writePulseP _keyP ma
    case ma of
        Nothing -> return []
        Just _  -> liftIO $ deRefWeaks _childrenP
evaluateNode (L lw) = do
    time           <- askTime
    LatchWrite{..} <- readRef lw
    mlatch         <- liftIO $ deRefWeak _latchLW -- retrieve destination latch
    case mlatch of
        Nothing    -> return ()
        Just latch -> do
            a <- _evalLW                    -- calculate new latch value
            -- liftIO $ Strict.evaluate a      -- see Note [LatchStrictness]
            rememberLatchUpdate $           -- schedule value to be set later
                modify' latch $ \l ->
                    a `seq` l { _seenL = time, _valueL = a }
    return []
evaluateNode (O o) = do
    debug "evaluateNode O"
    Output{..} <- readRef o
    m          <- _evalO                    -- calculate output action
    rememberOutput $ (o,m)
    return []

-- | Insert nodes into the queue
insertNodes :: RWS.Tuple BuildR (EvalPW, BuildW) Lazy.Vault -> [SomeNode] -> Queue SomeNode -> IO (Queue SomeNode)
insertNodes (RWS.Tuple (time,_) _ _) = go
    where
    go :: [SomeNode] -> Queue SomeNode -> IO (Queue SomeNode)
    go []              q = return q
    go (node@(P p):xs) q = do
        Pulse{..} <- readRef p
        if time <= _seenP
            then go xs q        -- pulse has already been put into the queue once
            else do             -- pulse needs to be scheduled for evaluation
                put p $! (let p = Pulse{..} in p { _seenP = time })
                go xs (Q.insert _levelP node q)
    go (node:xs)      q = go xs (Q.insert ground node q)
            -- O and L nodes have only one parent, so
            -- we can insert them at an arbitrary level
