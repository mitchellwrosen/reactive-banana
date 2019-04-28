{-# LANGUAGE ExistentialQuantification #-}

module Reactive.Banana.SomeNode
  ( SomeNode(..)
  , mkWeakNodeValue
  , printNode
  ) where

import Reactive.Banana.LatchWrite (LatchWrite)
import Reactive.Banana.Output (Output)
import Reactive.Banana.Pulse (Pulse, Pulse'(_nameP))
import Reactive.Banana.Ref (equalRef, mkWeakRefValue, readRef)

import Data.Hashable (Hashable, hashWithSalt)
import System.Mem.Weak (Weak)

data SomeNode
  = forall a. P (Pulse a)
  | L LatchWrite
  | O Output

instance Hashable SomeNode where
  hashWithSalt s (P x) = hashWithSalt s x
  hashWithSalt s (L x) = hashWithSalt s x
  hashWithSalt s (O x) = hashWithSalt s x

instance Eq SomeNode where
  (P x) == (P y) = equalRef x y
  (L x) == (L y) = equalRef x y
  (O x) == (O y) = equalRef x y

{-# INLINE mkWeakNodeValue #-}
mkWeakNodeValue :: SomeNode -> v -> IO (Weak v)
mkWeakNodeValue (P x) = mkWeakRefValue x
mkWeakNodeValue (L x) = mkWeakRefValue x
mkWeakNodeValue (O x) = mkWeakRefValue x

printNode :: SomeNode -> IO String
printNode (P p) = _nameP <$> readRef p
printNode (L l) = return "L"
printNode (O o) = return "O"
