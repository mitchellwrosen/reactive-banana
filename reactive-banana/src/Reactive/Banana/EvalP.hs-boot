{-# LANGUAGE RoleAnnotations #-}

module Reactive.Banana.EvalP
  ( EvalP
  , get
  , modify
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Vault.Lazy (Vault)

type role EvalP nominal
data EvalP a

instance Functor EvalP
instance Applicative EvalP
instance Monad EvalP
instance MonadIO EvalP

get :: EvalP Vault
modify :: (Vault -> Vault) -> EvalP ()
