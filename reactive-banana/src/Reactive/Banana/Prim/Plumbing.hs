{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecordWildCards, RecursiveDo, ScopedTypeVariables #-}
module Reactive.Banana.Prim.Plumbing where

import Reactive.Banana.Action (Action(..))
import Reactive.Banana.Build (Build, BuildIO, BuildR, BuildW(..))
import Reactive.Banana.EvalL (EvalL)
import Reactive.Banana.EvalO (EvalO, Future)
import Reactive.Banana.EvalP (EvalP)
import Reactive.Banana.EvalPW (EvalPW)
import Reactive.Banana.Latch (Latch, Latch'(..))
import Reactive.Banana.LatchWrite (LatchWrite'(..))
import Reactive.Banana.Level (ground)
import Reactive.Banana.Output (Output, Output'(..))
import Reactive.Banana.Prim.Types
import Reactive.Banana.Prim.Util
import Reactive.Banana.Pulse (Pulse, Pulse'(..), readPulseP)
import Reactive.Banana.Ref (mkWeakRefValue, newRef, put, readRef)
import Reactive.Banana.SomeNode (SomeNode(..))
import Reactive.Banana.Time (Time, agesAgo, beginning)

import qualified Reactive.Banana.Prim.Dependencies as Deps

import Control.Monad (join)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Function (on)
import Data.Functor
import Data.IORef
import Data.List (sortBy)
import Data.Monoid
import System.IO.Unsafe

import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.ReaderWriterIO as RW
import qualified Control.Monad.Trans.RWSIO as RWS
import qualified Data.Vault.Lazy as Lazy

{-----------------------------------------------------------------------------
    Build primitive pulses and latches
------------------------------------------------------------------------------}
-- | Make 'Pulse' from evaluation function
newPulse :: String -> EvalP (Maybe a) -> Build (Pulse a)
newPulse name eval = liftIO $ do
    key <- Lazy.newKey
    newRef $ Pulse
        { _keyP      = key
        , _seenP     = agesAgo
        , _evalP     = eval
        , _childrenP = []
        , _parentsP  = []
        , _levelP    = ground
        , _nameP     = name
        }

{-
* Note [PulseCreation]

We assume that we do not have to calculate a pulse occurrence
at the moment we create the pulse. Otherwise, we would have
to recalculate the dependencies *while* doing evaluation;
this is a recipe for desaster.

-}

-- | 'Pulse' that never fires.
neverP :: Build (Pulse a)
neverP = liftIO $ do
    key <- Lazy.newKey
    newRef $ Pulse
        { _keyP      = key
        , _seenP     = agesAgo
        , _evalP     = return Nothing
        , _childrenP = []
        , _parentsP  = []
        , _levelP    = ground
        , _nameP     = "neverP"
        }

-- | Return a 'Latch' that has a constant value
pureL :: a -> Latch a
pureL a = unsafePerformIO $ newRef $ Latch
    { _seenL  = beginning
    , _valueL = a
    , _evalL  = return a
    }

-- | Make new 'Latch' that can be updated by a 'Pulse'
newLatch :: forall a. a -> Build (Pulse a -> Build (), Latch a)
newLatch a = mdo
    latch <- liftIO $ newRef $ Latch
        { _seenL  = beginning
        , _valueL = a
        , _evalL  = do
            Latch {..} <- readRef latch
            RW.tell _seenL  -- indicate timestamp
            return _valueL  -- indicate value
        }
    let
        err        = error "incorrect Latch write"

        updateOn :: Pulse a -> Build ()
        updateOn p = do
            w  <- liftIO $ mkWeakRefValue latch latch
            lw <- liftIO $ newRef $ LatchWrite
                { _evalLW  = maybe err id <$> readPulseP p
                , _latchLW = w
                }
            -- writer is alive only as long as the latch is alive
            _  <- liftIO $ mkWeakRefValue latch lw
            (P p) `addChild` (L lw)

    return (updateOn, latch)

-- | Make a new 'Latch' that caches a previous computation.
cachedLatch :: EvalL a -> Latch a
cachedLatch eval = unsafePerformIO $ mdo
    latch <- newRef $ Latch
        { _seenL  = agesAgo
        , _valueL = error "Undefined value of a cached latch."
        , _evalL  = do
            Latch{..} <- liftIO $ readRef latch
            -- calculate current value (lazy!) with timestamp
            (a,time)  <- RW.listen eval
            liftIO $ if time <= _seenL
                then return _valueL     -- return old value
                else do                 -- update value
                    let _seenL  = time
                    let _valueL = a
                    a `seq` put latch (Latch {..})
                    return a
        }
    return latch

-- | Add a new output that depends on a 'Pulse'.
--
-- TODO: Return function to unregister the output again.
addOutput :: Pulse EvalO -> Build ()
addOutput p = do
    o <- liftIO $ newRef $ Output
        { _evalO = maybe (return $ debug "nop") id <$> readPulseP p
        }
    (P p) `addChild` (O o)
    RW.tell $ BuildW (mempty, [o], mempty, mempty)

{-----------------------------------------------------------------------------
    Build monad
------------------------------------------------------------------------------}
runBuildIO :: BuildR -> BuildIO a -> IO (a, Action, [Output])
runBuildIO i m = do
        (a, BuildW (topologyUpdates, os, liftIOLaters, _)) <- unfold mempty m
        doit $ liftIOLaters          -- execute late IOs
        return (a,Action $ Deps.buildDependencies topologyUpdates,os)
    where
    -- Recursively execute the  buildLater  calls.
    unfold :: BuildW -> BuildIO a -> IO (a, BuildW)
    unfold w m = do
        (a, BuildW (w1, w2, w3, later)) <- RW.runReaderWriterIOT m i
        let w' = w <> BuildW (w1,w2,w3,mempty)
        w'' <- case later of
            Just m  -> snd <$> unfold w' m
            Nothing -> return w'
        return (a,w'')

buildLater :: Build () -> Build ()
buildLater x = RW.tell $ BuildW (mempty, mempty, mempty, Just x)

-- | Pretend to return a value right now,
-- but do not actually calculate it until later.
--
-- NOTE: Accessing the value before it's written leads to an error.
--
-- FIXME: Is there a way to have the value calculate on demand?
buildLaterReadNow :: Build a -> Build a
buildLaterReadNow m = do
    ref <- liftIO $ newIORef $
        error "buildLaterReadNow: Trying to read before it is written."
    buildLater $ m >>= liftIO . writeIORef ref
    liftIO $ unsafeInterleaveIO $ readIORef ref

liftBuild :: Build a -> BuildIO a
liftBuild = id

getTimeB :: Build Time
getTimeB = (\(x,_) -> x) <$> RW.ask

alwaysP :: Build (Pulse ())
alwaysP = (\(_,x) -> x) <$> RW.ask

dependOn :: Pulse child -> Pulse parent -> Build ()
dependOn child parent = (P parent) `addChild` (P child)

keepAlive :: Pulse child -> Pulse parent -> Build ()
keepAlive child parent = liftIO $ mkWeakRefValue child parent >> return ()

addChild :: SomeNode -> SomeNode -> Build ()
addChild parent child =
    RW.tell $ BuildW (Deps.addChild parent child, mempty, mempty, mempty)

changeParent :: Pulse child -> Pulse parent -> Build ()
changeParent node parent =
    RW.tell $ BuildW (Deps.changeParent node parent, mempty, mempty, mempty)

liftIOLater :: IO () -> Build ()
liftIOLater x = RW.tell $ BuildW (mempty, mempty, Action x, mempty)

{-----------------------------------------------------------------------------
    EvalL monad
------------------------------------------------------------------------------}

getValueL :: Latch a -> EvalL a
getValueL latch = do
    Latch{..} <- readRef latch
    _evalL
