---
author: Me
date: Jul 31, 2026
tags: [haskell, free, monad]
---

# Notes on free monads

Free monads are things that I've heard about, and feel like I mostly get, but I haven't had an opportunity to use them _in anger_ to really cement the intuition. So I'm writing down a bunch of related thoughts and intuitions that have helped, to refer back to in lieu of experience.

## A tree with `f`-shaped branches

[Kmett says](https://www.reddit.com/r/haskell/comments/2znhjk/comment/cplwj3e/), "you can think of `Free f a` as a tree with an `f`-shaped branching structure." [Nate Faubion's "Free from tree" presentation](https://www.youtube.com/watch?v=eKkxmVFcd74) elaborates on this: plug in `Pair` for `f` and you've got a binary search tree (though more practically, you'd want `Compose Pair Maybe` to allow for trees with more than just an even number of branches at each node).

```haskell
data Pair a = Pair a a

data Free f a
  = Pure a
  | Roll (f (Free f a))

type BST a = Free Pair a

tree :: BST Int
tree =
  Roll (Pair
    (Roll (Pair
      (Pure 1)
      (Pure 3)))
    (Pure 7))
```

## Static analysis

One of the perceived benefits of expressing your program in terms of `Free` is that you might be able to statically analyze it (as a pure data structure), or do a "dry run". This is true for an AST built out of operators like

```haskell
data Operator a
  = Add a a
  | Sub a a
  | Mul a a
  | Neg a
```

But, it doesn't seem particularly useful to wrap `Operation` in `Free`? `do` notation doesn't add anything.

In the case where `f` in your `Free f` contains a continuation (`f` has any data constructors that take a lambda), for example

```haskell
data CmdF next
  = PutLine String next
  | GetLine (String -> next)
```

the program would not be introspectable, and you would need to provide all your inputs up front to do a dry run. Probably obvious, but something Kovanikov notes in his talk as a "weird thing" that gets overlooked in tutorials.

## `Monad` is to `Monoid` as `foldFree` is to `foldMap`

`List` is called the "free monoid", and `foldFree` is kinda like `foldMap` if you squint (and use squiggly arrow `~>` for natural transformations)

```haskell
foldMap
  :: Monoid m
  => (a -> m)
  -> FreeMonoid a  -- aka [a]
  -> m

foldFree
  :: Monad m
  => (f ~> m)
  -> FreeMonad f
  ~> m
```

Preserve your data (or AST) fully intact in the `Free*` structure, and then plug in different `a -> m` or `f ~> m` functions to get different interpretations of it.

## Performance

[There is a claim that Church-encoded free monads are "fast"](https://youtu.be/3GKQ4ni2pS0?si=e-18CYU-C711Yu6K&t=2371). From what I understand, the idea is that nested `>>=`'s are quadratic when using the ordinary `Free` type, but can be improved (to linear) when using Church encoding. There are maybe several possible Church encodings? The canonical reference is Janis Voigtländer's ["Asymptotic Improvement of Computations over Free Monads"](https://janis-voigtlaender.eu/papers/AsymptoticImprovementOfComputationsOverFreeMonads.pdf), and then Kmett has more to say in his ["Free monads for less" series](https://ekmett.github.io/reader/2011/free-monads-for-less/).

## freer

`Free`'s `Monad` instance requires a `Functor` constraint on `f`, `freer` somehow bypasses this using `Coyoneda`. See ["Freer Monads, More Extensible Effects"](https://okmij.org/ftp/Haskell/extensible/more.pdf).

<!-- Although `Free` is a tree (with `f`-shaped branches), in most cases you'll have a constructor for `f` that contains a lambda. -->

<!--


There's [a common pedagogical mistake](https://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy/) that teachers make when teaching about monads, which is to concentrate on some abstract intution that the teacher has reached, instead of the many concrete examples that helped them to reach that intuition. Something like mightybyte's [Monad Challenges](https://mightybyte.github.io/monad-challenges/pages/ex1-3.html) is a good example of the opposite approach: start with a few problems that you are likely to encounter in day-to-day programming (generating random numbers from a seed, handling sequences of failing computations, exploring all possible combinations of some set of values) and showing how they are similar and that the "monad" concept abstracts over all these use cases. The intuition is hardened by repeated exposure to similar examples, and

  -->
  <!--


Pretty early on in my Haskell-learning, I heard about "free monads" and was mystified. I bounced off some Kmett blog posts, and when I asked for a simple explanation or some more beginner-friendly material to read up on (in the haskell-beginners Slack) I was told something like "it's a tree of `bind`s", but that didn't help my understanding at all. I continued to bounce off of different blog posts and explanations, playing out the [monad tutorial fallacy](), but with nothing really concrete to sink my teeth into (I have had plenty use cases for _plain_ monads, but no real uses for free monads after all).

I have struggled for a while with wrapping my head around what "free monad" means. I'm not even sure how to formulate that last sentence: is it "_the_ free monad"? or "free monads" (plural)? I have no idea, and will likely continue to mix those up through the rest of this piece.

I have read Kmett's lengthy Reddit post,

There's a understanding of "free monad" (or free structures in general) that I've latched onto. I should disclaim I don't really get the math, my intuition is rooted in Haskell, and so code examples and explanations will use Haskell.

## Free ... as in beer?

Why is it called a _"free"_ monad? Is it, "free as in beer"? "Free as in speech"? "Free as in \[some other thing\]"?

To answer this, I think it's helpful to look at some other "free" things. Haskellers are also known to say, "`List` is the free `Monoid`". Can we draw some generalizations from these two examples? `List` and `Free` are type constructors, `Monoid` and `Monad` are type classes. So I'm going to assert this: when a Haskeller says, **"`Foo` is the free `Bloop`"**, they are saying the following:

1. `Foo` is a _type constructor_ (meaning, it takes some other type as input, and returns a type). Specifically, `Foo` has kind `inkind → outkind` for some kinds `inkind` & `outkind`.
2. `Bloop` is a _type class_ for types of kind `outkind` (the kind that you get from partially-applying `Foo`)
3. You can write a `Bloop` instance for `Foo` applied to any input type of kind `inkind` (without knowing anything about that input type)

In **"`List` is the free `Monoid`"**. `List` is a type constructor, and I can write a `Monoid` instance for `List a` _for any type `a`,_ without knowing anything about `a`. In other words, the `Monoid` instance doesn't use anything specific to `a`, it just uses `List` operations.

```haskell
instance Monoid (List item) where
  (<>)   = (++)
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
 -->
