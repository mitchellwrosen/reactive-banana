module Reactive.Banana.EvalL
  ( EvalL
  ) where

import Reactive.Banana.Time (Time)

import Control.Monad.Trans.ReaderWriterIO (ReaderWriterIOT)


-- Computation with a timestamp that indicates the last time it was performed.
type EvalL
  = ReaderWriterIOT () Time IO
