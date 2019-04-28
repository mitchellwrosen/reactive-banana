{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE ExistentialQuantification, FlexibleInstances, NamedFieldPuns,
             TypeSynonymInstances #-}
module Reactive.Banana.Prim.Types where

import Reactive.Banana.Output (Output)
import Reactive.Banana.Prim.Graph (Graph)
import Reactive.Banana.Prim.OrderedBag as OB (OrderedBag, empty)
import Reactive.Banana.Prim.Util
import Reactive.Banana.Time (Time)

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.ReaderWriterIO
import           Control.Monad.Trans.RWSIO
import           Data.Functor
import           Data.Hashable
import           Data.Monoid (Monoid, mappend, mempty)
import           Data.Semigroup
import qualified Data.Vault.Lazy as Lazy
import           System.IO.Unsafe
import           System.Mem.Weak


-- type EvalNetwork a = Network -> IO (a, Network)
-- type Step          = EvalNetwork (IO ())
