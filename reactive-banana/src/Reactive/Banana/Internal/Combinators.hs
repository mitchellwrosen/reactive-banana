{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables, RecursiveDo, FlexibleInstances, NoMonomorphismRestriction #-}
module Reactive.Banana.Internal.Combinators where

import           Control.Concurrent.MVar
import           Control.Event.Handler
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class           (lift)
import           Control.Monad.Trans.Reader
import           Data.Coerce
import           Data.Functor
import           Data.Functor.Identity
import           Data.IORef
import qualified Reactive.Banana.Prim        as Prim
import qualified Reactive.Banana.Prim.Types  as Prim
import           Reactive.Banana.Prim.Cached

type Build   = Prim.Build
type Latch a = Prim.Latch a
type Pulse a = Prim.Pulse a
type Future  = Prim.Future

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}
-- type Behavior a = Cached Moment (Latch a, Pulse ())
type Event a    = Cached Moment (Pulse a)
type Moment     = ReaderT EventNetwork Build

liftBuild :: Build a -> Moment a
liftBuild = lift

{-----------------------------------------------------------------------------
    Interpretation
------------------------------------------------------------------------------}
interpret :: (Event a -> Moment (Event b)) -> [Maybe a] -> IO [Maybe b]
interpret f = Prim.interpret $ \pulse -> runReaderT (g pulse) undefined
    where
    g pulse = runCached =<< f (fromPure pulse)
    -- Ignore any  addHandler  inside the  Moment

{-----------------------------------------------------------------------------
    IO
------------------------------------------------------------------------------}
-- | Data type representing an event network.
data EventNetwork = EventNetwork
  { runStep :: Prim.Step -> IO ()
  , actuate :: IO ()
  , pause   :: IO ()
  }

-- | Compile to an event network.
compile :: Moment () -> IO EventNetwork
compile setup = do
    actuated <- newIORef False                   -- flag to set running status
    s        <- newEmptyMVar                     -- setup callback machinery
    let
        whenFlag flag action = readIORef flag >>= \b -> when b action
        runStep f            = whenFlag actuated $ do
            s1 <- takeMVar s                    -- read and take lock
            -- pollValues <- sequence polls     -- poll mutable data
            (output, s2) <- f s1                -- calculate new state
            putMVar s s2                        -- write state
            output                              -- run IO actions afterwards

        eventNetwork = EventNetwork
            { runStep = runStep
            , actuate = writeIORef actuated True
            , pause   = writeIORef actuated False
            }

    (output, s0) <-                             -- compile initial graph
        Prim.compile (runReaderT setup eventNetwork) Prim.emptyNetwork
    putMVar s s0                                -- set initial state

    return $ eventNetwork

fromAddHandler
  :: AddHandler a
  -> Moment (Cached Moment (Pulse a))
fromAddHandler addHandler = do
    (p, fire) <- liftBuild $ Prim.newInput
    network   <- ask
    liftIO $ register addHandler $ runStep network . fire
    return (fromPure p)

addReactimate
  :: Cached Moment (Pulse (Future (IO ())))
  -> Moment ()
addReactimate e = do
    network   <- ask
    liftBuild $ Prim.buildLater $ do
        -- Run cached computation later to allow more recursion with `Moment`
        p <- runReaderT (runCached e) network
        Prim.addHandler p id

fromPoll
  :: IO a
  -> Moment (Cached Moment (Latch a, Pulse ()))
fromPoll poll = do
    a <- liftIO poll
    e <- liftBuild $ do
        p <- Prim.unsafeMapIOP (const poll) =<< Prim.alwaysP
        return (fromPure p)
    stepperB a e

liftIONow :: IO a -> Moment a
liftIONow =
  liftIO

liftIOLater :: IO () -> Moment ()
liftIOLater =
  liftBuild . Prim.liftIOLater

imposeChanges
  :: Cached Moment (Latch a, Pulse ())
  -> Cached Moment (Pulse ())
  -> Cached Moment (Latch a, Pulse ())
imposeChanges b e =
  cache $ do
    (l1, _) <- runCached b
    p2 <- runCached e
    pure (l1, p2)

{-----------------------------------------------------------------------------
    Combinators - basic
------------------------------------------------------------------------------}

never :: Cached Moment (Pulse a)
never =
  don'tCache Prim.neverP

unionWith
  :: (a -> a -> a)
  -> Cached Moment (Pulse a)
  -> Cached Moment (Pulse a)
  -> Cached Moment (Pulse a)
unionWith f e1 e2 =
  cache $ do
    p1 <- runCached e1
    p2 <- runCached e2
    lift (Prim.unionWithP f p1 p2)

filterJust
  :: Cached Moment (Pulse (Maybe a))
  -> Cached Moment (Pulse a)
filterJust e =
  cache $ do
    p <- runCached e
    lift (Prim.filterJustP p)

mapE
  :: (a -> b)
  -> Cached Moment (Pulse a)
  -> Cached Moment (Pulse b)
mapE f e =
  cache $ do
    p <- runCached e
    lift (Prim.mapP f p)

applyE
  :: Cached Moment (Latch (a -> b), Pulse ())
  -> Cached Moment (Pulse a)
  -> Cached Moment (Pulse b)
applyE b e =
  cache $ do
    ~(l, _) <- runCached b
    p <- runCached e
    lift (Prim.applyP l p)

changesB
  :: Cached Moment (Latch a, Pulse ())
  -> Cached Moment (Pulse (Future a))
changesB b =
  cache $ do
    ~(l, p) <- runCached b
    liftBuild (Prim.tagFuture l p)

pureB :: a -> Cached Moment (Latch a, Pulse ())
pureB a =
  cache $ do
    p <- runCached never
    l <- Prim.pureL a
    pure (l, p)

applyB
  :: Cached Moment (Latch (a -> b), Pulse ())
  -> Cached Moment (Latch a, Pulse ())
  -> Cached Moment (Latch b, Pulse ())
applyB bf bx =
  cache $ do
    ~(lf, cf) <- runCached bf
    ~(lx, cx) <- runCached bx
    lift $ do
      cy <- Prim.unionWithP const cf cx
      ly <- Prim.applyL lf lx
      pure (ly, cy)

mapB
  :: (a -> b)
  -> Cached Moment (Latch a, Pulse ())
  -> Cached Moment (Latch b, Pulse ())
mapB f =
  applyB (pureB f)

{-----------------------------------------------------------------------------
    Combinators - accumulation
------------------------------------------------------------------------------}
-- Make sure that the cached computation (Event or Behavior)
-- is executed eventually during this moment.
trim :: Cached Moment a -> Moment (Cached Moment a)
trim b = do
  liftBuildFun Prim.buildLater $ void $ runCached b
  return b

-- Cache a computation at this moment in time
-- and make sure that it is performed in the Build monad eventually
cacheAndSchedule :: Moment a -> Moment (Cached Moment a)
cacheAndSchedule m = ask >>= \r -> liftBuild $ do
    let c = cache (const m r)   -- prevent let-floating!
    Prim.buildLater $ void $ runReaderT (runCached c) r
    return c

stepperB
  :: forall a.
     a
  -> Cached Moment (Pulse a)
  -> Moment (Cached Moment (Latch a, Pulse ()))
stepperB a e =
  cacheAndSchedule $ do
    p0 :: Pulse a <- runCached e
    lift $ do
      p1 :: Pulse (a -> a) <- Prim.mapP const p0
      p2 :: Pulse () <- Prim.mapP (const ()) p1
      (l,_) <- Prim.accumL a p1
      return (l,p2)

accumE
  :: a
  -> Cached Moment (Pulse (a -> a))
  -> Moment (Cached Moment (Pulse a))
accumE a e1 =
  cacheAndSchedule $ do
    p0 :: Pulse (a -> a) <-
      runCached e1

    lift $ do
      (_, p1) <- Prim.accumL a p0
      return p1

{-----------------------------------------------------------------------------
    Combinators - dynamic event switching
------------------------------------------------------------------------------}

liftBuildFun :: (Build a -> Build b) -> Moment a -> Moment b
liftBuildFun =
    mapReaderT

valueB
  :: Cached Moment (Latch a, Pulse ())
  -> Moment a
valueB b = do
    ~(l, _) <- runCached b
    lift (Prim.readLatch l)

valueBLater
  :: Cached Moment (Latch a, Pulse ())
  -> Moment a
valueBLater b =
  liftBuildFun Prim.buildLaterReadNow (valueB b)

executeP :: Pulse (Moment a) -> Moment (Pulse a)
executeP p1 = do
  r <- ask
  lift $ do
    p2 <- Prim.mapP runReaderT p1
    Prim.executeP p2 r

observeE
  :: Cached Moment (Pulse (Moment a))
  -> Cached Moment (Pulse a)
observeE e =
  cache (executeCachedP e)

executeE
  :: Cached Moment (Pulse (Moment a))
  -> Moment (Cached Moment (Pulse a))
executeE e = do
    -- Run cached computation later to allow more recursion with `Moment`
    p :: Pulse a <-
      liftBuildFun Prim.buildLaterReadNow (executeCachedP e)
    pure (fromPure p)

executeCachedP :: Cached Moment (Pulse (Moment a)) -> Moment (Pulse a)
executeCachedP = runCached >=> executeP

switchE
  :: Cached Moment (Pulse (Cached Moment (Pulse a)))
  -> Moment (Cached Moment (Pulse a))
switchE e =
  ask >>= \r ->
    cacheAndSchedule $ do
      p1 :: Pulse (Cached Moment (Pulse a)) <-
        runCached e
      liftBuild $ do
        p2 :: Pulse (EventNetwork -> Build (Pulse a)) <-
          Prim.mapP (runReaderT . runCached) p1
        p3 :: Pulse (Pulse a) <-
          Prim.executeP p2 r
        Prim.switchP p3

switchB
  :: Cached Moment (Latch a, Pulse ())
  -> Cached Moment (Pulse (Cached Moment (Latch a, Pulse ())))
  -> Moment (Cached Moment (Latch a, Pulse ()))
switchB b e =
  ask >>= \r ->
    cacheAndSchedule $ do
      ~(l0, p0) :: (Latch a, Pulse ()) <-
        runCached b

      p1 :: Pulse (Cached Moment (Latch a, Pulse ())) <-
        runCached e

      liftBuild $ do
        p2 :: Pulse (EventNetwork -> Build (Latch a, Pulse ())) <-
          Prim.mapP (runReaderT . runCached) p1

        p3 :: Pulse (Latch a, Pulse ()) <-
          Prim.executeP p2 r

        lr :: Latch a <-
          Prim.switchL l0 =<< Prim.mapP fst p3

        -- TODO: switch away the initial behavior
        let c1 = p0                              -- initial behavior changes
        c2 <- Prim.mapP (const ()) p3            -- or switch happens
        c3 <- Prim.switchP =<< Prim.mapP snd p3  -- or current behavior changes
        pr <- merge c1 =<< merge c2 c3
        return (lr, pr)

merge :: Pulse () -> Pulse () -> Build (Pulse ())
merge = Prim.unionWithP (\_ _ -> ())
