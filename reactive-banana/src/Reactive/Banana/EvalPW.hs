module Reactive.Banana.EvalPW
  ( EvalPW
  ) where

import Reactive.Banana.EvalLW (EvalLW)
import Reactive.Banana.EvalO (EvalO)
import Reactive.Banana.Output (Output)


type EvalPW =
  (EvalLW, [(Output, EvalO)])
