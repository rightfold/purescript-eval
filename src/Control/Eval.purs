module Control.Eval
( class Eval
, defer
, force

, Strict
, Replay
) where

import Data.Lazy (Lazy)
import Data.Lazy as Lazy
import Prelude

class Eval f where
  defer :: forall a. (Unit -> a) -> f a
  force :: forall a. f a -> a

instance evalLazy :: Eval Lazy where
  defer = Lazy.defer
  force = Lazy.force

newtype Strict a = Strict a

instance evalStrict :: Eval Strict where
  defer t = Strict (t unit)
  force (Strict x) = x

newtype Replay a = Replay (Unit -> a)

instance evalReplay :: Eval Replay where
  defer t = Replay t
  force (Replay t) = t unit
