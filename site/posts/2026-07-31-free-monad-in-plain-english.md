---
author: Me
date: Jul 31, 2026
tags: [haskell, free, monad]
---

# Free monad in plain English

Here's a probably imprecise understanding of "free monad" (or free structures in general) that I'm still shaping. I should disclaim I don't really get the math, my intuition is rooted in Haskell, and so code examples and explanations will use Haskell.

## Free ... as in beer?

Why is it called a _"free"_ monad? Is it, "free as in beer"? "Free as in speech"? "Free as in \[some other thing\]"?

To answer this, I think it's helpful to look at some other "free" things. Haskellers are also known to say, "`List` is the free `Monoid`". Can we draw some generalizations from these two examples? `List` and `Free` are type constructors, `Monoid` and `Monad` are type classes. So I'm going to assert this: when a Haskeller says, **"`Foo` is the free `Bloop`"**, they are saying the following:

1. `Foo` is a _type constructor_ (meaning, it takes some other type as input, and returns a type). Specifically, `Foo` has kind `inkind → outkind` for some kinds `inkind` & `outkind`.
2. `Bloop` is a _type class_ for types of kind `outkind` (the kind that you get from partially-applying `Foo`)
3. You can write a `Bloop` instance for `Foo` applied to any input type of kind `inkind` (without knowing anything about that input type)

In **"`List` is the free `Monoid`"**. `List` is a type constructor, and I can write a `Monoid` instance for `List a` _for any type `a`,_ without knowing anything about `a`. In other words, the `Monoid` instance doesn't use anything specific to `a`, it just uses `List` operations.

```haskell
instance Monoid (List item) where
  (<>) = (++)
  mempty = []
```

What are some other ones?

- `NonEmptyList` is the free `Semigroup` for similar reasons
- `Maybe` is the free `Default` because I can choose `def = Nothing` for any type
- `Compose Set List` is the free `Semiring`

So I think of this as "free as in beer": these are instances you get "for free" just by putting `a` into the structure of the type constructor, you don't need any further constraints on the `a`.

Note that "`Free` is the free `Monad`" is an outlier in a few ways:

- the `inkind` kind is `Type → Type` (the other examples have `inkind ∷ Type`)—i.e. it's a type constructor that takes a type constructor
- "free" is the name of the data type, but really it's not any _more_ free than the other types listed above—this seems like plain bad naming to me
- in fact, it's not even quite _as_ free as the others—it requires a `Functor` constraint on its input type (more on this below)

## Why though

This doesn't really satisfy the question of _why_ you might want to do this—what are you getting for free by using any of these free structures?

Here's a dumb but maybe illustrative example. Say you regularly play _Super Smash Bros_ (the N64 game) with your friends. You keep track of who's the best player by who has had the most KOs in a game. Maybe you write this down on a white board. As soon as someone scores higher than the current champion, you clear the board and write the new champion's name and score.

As more games are played, there's a tie for highest score. To break the tie, you'd like to change the criteria for best player: instead of all-time highest KOs, it should be whoever has won the most games total. Too bad! Nobody remembers how many games each player has won, all you've kept track of is KOs.

What you could have done instead, is kept a log of all the scores of each game (a `List Results`, where `Results` contains the game results: place/ranking and KOs per player). That way you can calculate the best player afterwards, using whatever criteria you'd like.

Calculating best player in different ways is like plugging in a different `Monoid` in a `foldMap` over the `List Result` log.

```haskell
foldMap
  :: Monoid m
  => (a -> m) -- this lets you pick the criteria
  -> [a]      -- your logs
  -> m        -- your result (best player)
```

If you use a function that turns `Results` into `(Max Int, First Player)` as your `(a -> m)` that's your max KOs, if you use `Results -> Map Player (Sum Int)` that would show you who has won the most games, etc.

There's a similar thing in `Free` called `foldFree`

```haskell
foldFree
  :: Monad m
  => (forall x. f x -> m x) -- this lets you pick how your AST is interpreted
  -> Free f a               -- your AST
  -> m a                    -- your result
```

Again, the naming kinda gets in the way: `foldFree` seems to be doing just as much "mapping" as `foldMap`, and as we saw both `Free` and `[]` are "free".

Aside: Hm, maybe this is "free as in speech" after all?

## Freer

Remember when we said that `Free` was an outlier because it required a `Functor` constraint on `f`? Turns out you can get `f`'s `Functor` instance "for free" (I think?) using something called `Coyoneda`. I don't yet understand what this is about, but apparently it's in this paper: https://okmij.org/ftp/Haskell/extensible/more.pdf
