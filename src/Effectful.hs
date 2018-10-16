{-# LANGUAGE InstanceSigs #-}
module Effectful where

{-| This type represents the result of a computation that could have emmited
effects of type `e`.

The Applicative and Monad instances take care of combining computations while
automatically collecting all effects to avoid boilerplate.

For example, if using plain tuples we might run into code like this:

  let
    (r1, effs)   = foo
    (r2, effs')  = bar r1
    (r3, effs'') = baz
  in
    (final r2 r3, concat [effs, effs', effs''])

Which can be written as follows using this type:

  do
    r1 <- foo
    r2 <- bar r2
    r3 <- baz
    return (final r2 r3)

-}
newtype Effectful e a = Eff { runEff :: (a , [e]) }

instance Functor (Effectful e) where
  fmap :: (a -> b) -> Effectful e a -> Effectful e b
  fmap f (Eff (a, es)) = Eff (f a, es)

instance Applicative (Effectful e) where
  pure :: a -> Effectful e a
  pure a = Eff (a, [])

  (<*>) :: Effectful e (a -> b) -> Effectful e a -> Effectful e b
  (<*>) (Eff (f, es)) (Eff (a, es')) = Eff (f a, es ++ es')

instance Monad (Effectful e) where
  return :: a -> Effectful e a
  return = pure

  (>>=) :: Effectful e a -> (a -> Effectful e b) -> Effectful e b
  (>>=) (Eff (a, es)) f = Eff $
    let
      (b, es') = runEff (f a)
    in
      (b, es ++ es')

withEffects :: a -> [e] -> Effectful e a
withEffects a effects = Eff (a, effects)

performing :: e -> Effectful e a -> Effectful e a
performing effect (Eff (a, effects)) = Eff (a, effects ++ [effect])

(!) :: Effectful e a -> e -> Effectful e a
(!) = flip performing

{-| Runs the sequence of effects using the provided function. If one of those
fail by returning `False`, then the rest of the effects will not be run.
-}
run :: (e -> IO Bool) -> Effectful e a -> IO a
run _ (Eff (a, []      )) = return a
run f (Eff (a, (e : es))) = do
  success <- f e
  if success then run f (Eff (a, es)) else return a
