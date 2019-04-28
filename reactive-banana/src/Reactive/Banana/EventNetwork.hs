module Reactive.Banana.EventNetwork
  ( EventNetwork(..)
  , actuate
  , pause
  ) where

import Reactive.Banana.Network (Network)


-- | Data type that represents a compiled event network.
-- It may be paused or already running.
data EventNetwork
  = EventNetwork
  { runStep :: (Network -> IO (IO (), Network)) -> IO ()
  , _actuate :: IO ()
  , _pause   :: IO ()
  }

-- | Actuate an event network.
-- The inputs will register their event handlers, so that
-- the networks starts to produce outputs in response to input events.
actuate :: EventNetwork -> IO ()
actuate =
  _actuate

-- | Pause an event network.
-- Immediately stop producing output.
-- (In a future version, it will also unregister all event handlers for inputs.)
-- Hence, the network stops responding to input events,
-- but it's state will be preserved.
--
-- You can resume the network with 'actuate'.
--
-- Note: You can stop a network even while it is processing events,
-- i.e. you can use 'pause' as an argument to 'reactimate'.
-- The network will /not/ stop immediately though, only after
-- the current event has been processed completely.
pause :: EventNetwork -> IO ()
pause =
  _pause
