# purescript-eval

purescript-eval provides utilities for abstracting over evaluation strategies.
This is useful for data structures where you want the user to decide when their
expressions will be evaluated. Here is an example:

```purescript
data Nat e = Z | S (e (Nat e))

fromInt :: forall e. (Eval e) => Int -> Nat e
fromInt 0 = Z
fromInt n = S (defer \_ -> fromInt (n - 1))

toInt :: forall e. (Eval e) => Nat e -> Int
toInt Z = 0
toInt (S n) = 1 + toInt (force n)
```
