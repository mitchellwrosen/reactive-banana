module Reactive.Banana.Moment
  ( Moment
  , compile
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.IORef

import Reactive.Banana.Build (Build)
import Reactive.Banana.EventNetwork (EventNetwork(..))
import Reactive.Banana.Network (emptyNetwork)

import qualified Reactive.Banana.Prim as Prim (compile)



type Moment
  = ReaderT EventNetwork Build

-- | Compile the description of an event network
-- into an 'EventNetwork'
-- that you can 'actuate', 'pause' and so on.
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
            , _actuate = writeIORef actuated True
            , _pause   = writeIORef actuated False
            }

    (output, s0) <-                             -- compile initial graph
        Prim.compile (runReaderT setup eventNetwork) emptyNetwork
    putMVar s s0                                -- set initial state

    return $ eventNetwork
