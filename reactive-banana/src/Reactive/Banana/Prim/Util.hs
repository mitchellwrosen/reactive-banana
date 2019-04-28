{-# LANGUAGE CPP #-}
{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Reactive.Banana.Prim.Util where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Hashable
import           Data.IORef
import           Data.Maybe                    (catMaybes)
import           Data.Unique.Really
import qualified GHC.Base               as GHC
import qualified GHC.IORef              as GHC
import qualified GHC.STRef              as GHC
import qualified GHC.Weak               as GHC
import           System.Mem.Weak

debug :: MonadIO m => String -> m ()
-- debug = liftIO . putStrLn
debug _ = return ()

nop :: Monad m => m ()
nop = return ()

{-----------------------------------------------------------------------------
    Weak pointers
------------------------------------------------------------------------------}
-- | Dereference a list of weak pointers while discarding dead ones.
deRefWeaks :: [Weak v] -> IO [v]
deRefWeaks ws = {-# SCC deRefWeaks #-} fmap catMaybes $ mapM deRefWeak ws
