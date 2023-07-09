---
author: Me
date: May 30, 2023
tags: [haskell, purescript, cont]
---

# A syntax-driven intuition for Cont

Based on the [internal accessor `(>>-)`](https://hackage.haskell.org/package/managed-1.0.10/docs/src/Control.Monad.Managed.html#Managed) from Gabriella Gonzalez's `managed` library. _Edit:_ I re-discovered that Gabriella had [posted about "renaming things" like this](https://www.haskellforall.com/2014/04/how-continuation-monad-works.html) to get an intuition for `ContT` on her blog. She also has another relevant post [explaining how to use `Cont` in depth](https://www.haskellforall.com/2012/12/the-continuation-monad.html).

PureScript allows us to override the default `bind` implementation and re-purpose the `do` syntax sugar (we could do the same in Haskell with `RebindableSyntax` enabled).

<Multicodeblock>

```[PureScript (with infix operator)]purescript
import Prelude

newtype Cont r a = Cont ((a -> r) -> r)

runCont :: forall r a. Cont r a -> (a -> r) -> r
runCont (Cont f) = f

infixl 1 runCont as >>-

instance Functor (Cont r) where
  map f ma =
    Cont \pure' ->
      ma >>- \a ->
        pure' (f a)

instance Apply (Cont r) where
  apply mab ma =
    Cont \pure' -> do
      mab >>- \ab ->
        ma >>- \a ->
          pure' (ab a)

instance Applicative (Cont r) where
  pure a =
    Cont \pure' ->
      pure' a

instance Bind (Cont r) where
  bind ma amb =
    Cont \pure' ->
      ma >>- \a ->
        amb a >>- \b ->
          pure' b
```

```[PureScript (with do)]purescript
import Prelude hiding (bind)

newtype Cont r a = Cont ((a -> r) -> r)

bind :: forall r a. Cont r a -> (a -> r) -> r
bind (Cont f) = f



instance Functor (Cont r) where
  map f ma =
    Cont \pure' -> do
      a <- ma
      pure' (f a)

instance Apply (Cont r) where
  apply mab ma =
    Cont \pure' -> do
      ab <- mab
      a <- ma
      pure' (ab a)

instance Applicative (Cont r) where
  pure a =
    Cont \pure' ->
      pure' a

instance Bind (Cont r) where
  bind ma amb =
    Cont \pure' -> do
      a <- ma
      b <- amb a
      pure' b
```

</Multicodeblock>
