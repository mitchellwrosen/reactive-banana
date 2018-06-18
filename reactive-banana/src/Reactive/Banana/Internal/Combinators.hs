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
type Behavior a = Cached Moment (Latch a, Pulse ())
type Event a    = Cached Moment (Pulse a)
type Moment     = ReaderT EventNetwork Prim.Build

liftBuild :: Build a -> Moment a
liftBuild = lift

{-----------------------------------------------------------------------------
    Interpretation
------------------------------------------------------------------------------}
interpret :: (Event a -> Moment (Event b)) -> [Maybe a] -> IO [Maybe b]
interpret f = Prim.interpret $ \pulse -> runReaderT (g pulse) undefined
    where
    g pulse = runCached =<< f (Prim.fromPure pulse)
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

fromAddHandler :: AddHandler a -> Moment (Event a)
fromAddHandler addHandler = do
    (p, fire) <- liftBuild $ Prim.newInput
    network   <- ask
    liftIO $ register addHandler $ runStep network . fire
    return $ Prim.fromPure p

addReactimate :: Event (Future (IO ())) -> Moment ()
addReactimate e = do
    network   <- ask
    liftBuild $ Prim.buildLater $ do
        -- Run cached computation later to allow more recursion with `Moment`
        p <- runReaderT (runCached e) network
        Prim.addHandler p id

fromPoll :: IO a -> Moment (Behavior a)
fromPoll poll = do
    a <- liftIO poll
    e <- liftBuild $ do
        p <- Prim.unsafeMapIOP (const poll) =<< Prim.alwaysP
        return $ Prim.fromPure p
    stepperB a e

liftIONow :: IO a -> Moment a
liftIONow = liftIO

liftIOLater :: IO () -> Moment ()
liftIOLater = lift . Prim.liftBuild . Prim.liftIOLater

imposeChanges :: Behavior a -> Event () -> Behavior a
imposeChanges = liftCached2 $ \(l1,_) p2 -> return (l1,p2)

{-----------------------------------------------------------------------------
    Combinators - basic
------------------------------------------------------------------------------}
never :: Event a
never =
  don'tCache (lift Prim.neverP)

unionWith :: (a -> a -> a) -> Event a -> Event a -> Event a
unionWith f event1 event2 =
  cache $ do
    pulse1 <- runCached event1
    pulse2 <- runCached event2
    lift (Prim.unionWithP f pulse1 pulse2)

filterJust :: Event (Maybe a) -> Event a
filterJust event =
  cache $ do
    pulse <- runCached event
    lift (Prim.filterJustP pulse)

mapE :: (a -> b) -> Event a -> Event b
mapE f event = cache $ do
  pulse <- runCached event
  lift (Prim.mapP f pulse)

applyE :: Behavior (a -> b) -> Event a -> Event b
applyE behavior event =
  cache $ do
    ~(latch, _) <- runCached behavior
    pulse <- runCached event
    lift (Prim.applyP latch pulse)

changesB :: Behavior a -> Event (Future a)
changesB behavior =
  cache $ do
    ~(latch, pulse) <- runCached behavior
    liftBuild (Prim.tagFuture latch pulse)

pureB :: a -> Behavior a
pureB a =
  cache $ do
    p <- runCached never
    pure (Prim.pureL a, p)

applyB :: Behavior (a -> b) -> Behavior a -> Behavior b
applyB bf bx =
  cache $ do
    ~(lf, cf) <- runCached bf
    ~(lx, cx) <- runCached bx
    lift $ do
      cy <- Prim.unionWithP const cf cx
      let ly = Prim.applyL lf lx
      pure (ly, cy)

mapB :: (a -> b) -> Behavior a -> Behavior b
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

stepperB :: forall a. a -> Event a -> Moment (Behavior a)
stepperB a e =
  cacheAndSchedule $ do
    p0 :: Pulse a <- runCached e
    lift $ do
      p1 :: Pulse (a -> a) <- Prim.mapP const p0
      p2 :: Pulse () <- Prim.mapP (const ()) p1
      (l,_) <- Prim.accumL a p1
      return (l,p2)

accumE :: a -> Event (a -> a) -> Moment (Event a)
accumE a e1 =
  cacheAndSchedule $ do
    p0 :: Pulse (a -> a) <- runCached e1
    lift $ do
      (_, p1) <- Prim.accumL a p0
      return p1

{-----------------------------------------------------------------------------
    Combinators - dynamic event switching
------------------------------------------------------------------------------}
liftBuildFun :: (Build a -> Build b) -> Moment a -> Moment b
liftBuildFun f m = do
    r <- ask
    liftBuild $ f $ runReaderT m r

valueB :: Behavior a -> Moment a
valueB b = do
    ~(l,_) <- runCached b
    lift (Prim.readLatch l)

valueBLater :: Behavior a -> Moment a
valueBLater = liftBuildFun Prim.buildLaterReadNow . valueB

executeP :: Pulse (Moment a) -> Moment (Pulse a)
executeP p1 = do
  r <- ask
  lift $ do
    p2 <- Prim.mapP runReaderT p1
    Prim.executeP p2 r

observeE :: Event (Moment a) -> Event a
observeE = liftCached1 $ executeP

executeE :: Event (Moment a) -> Moment (Event a)
executeE e = do
    -- Run cached computation later to allow more recursion with `Moment`
    p <- liftBuildFun Prim.buildLaterReadNow $ executeP =<< runCached e
    return $ fromPure p

switchE :: Event (Event a) -> Moment (Event a)
switchE e = ask >>= \r -> cacheAndSchedule $ do
    p1 <- runCached e
    liftBuild $ do
        p2 <- Prim.mapP (runReaderT . runCached) p1
        p3 <- Prim.executeP p2 r
        Prim.switchP p3

switchB :: Behavior a -> Event (Behavior a) -> Moment (Behavior a)
switchB b e = ask >>= \r -> cacheAndSchedule $ do
    ~(l0,p0) <- runCached b
    p1       <- runCached e
    liftBuild $ do
        p2 <- Prim.mapP (runReaderT . runCached) p1
        p3 <- Prim.executeP p2 r

        lr <- Prim.switchL l0 =<< Prim.mapP fst p3
        -- TODO: switch away the initial behavior
        let c1 = p0                              -- initial behavior changes
        c2 <- Prim.mapP (const ()) p3            -- or switch happens
        c3 <- Prim.switchP =<< Prim.mapP snd p3  -- or current behavior changes
        pr <- merge c1 =<< merge c2 c3
        return (lr, pr)

merge :: Pulse () -> Pulse () -> Build (Pulse ())
merge = Prim.unionWithP (\_ _ -> ())
