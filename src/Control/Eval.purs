module Control.Eval
( class Eval
, defer
, force

, forceApply, ($!)
, forceApplyFlipped, (#!)

, Lazy
, Strict
, Replay
) where

import Data.Lazy as Lazy
import Prelude

-- | Evaluation strategy.
class Eval f where
  defer :: forall a. (Unit -> a) -> f a
  force :: forall a. f a -> a

-- | Lazy evaluation. `force` calls the thunk once, then memorizes the result.
type Lazy = Lazy.Lazy

instance evalLazy :: Eval Lazy.Lazy where
  defer = Lazy.defer
  force = Lazy.force

-- | Strict evaluation. `defer` immediately calls the thunk.
newtype Strict a = Strict a

instance evalStrict :: Eval Strict where
  defer t = Strict (t unit)
  force (Strict x) = x

-- | By-name evaluation. `force` calls the thunk every time.
newtype Replay a = Replay (Unit -> a)

instance evalReplay :: Eval Replay where
  defer t = Replay t
  force (Replay t) = t unit

forceApply :: forall f a b. (Eval f) => (a -> b) -> f a -> b
forceApply f x = f (force x)

forceApplyFlipped :: forall f a b. (Eval f) => f a -> (a -> b) -> b
forceApplyFlipped = flip forceApply

infixr 0 forceApply as $!
infixl 1 forceApplyFlipped as #!
