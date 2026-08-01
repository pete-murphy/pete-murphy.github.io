---
author: Me
date: Apr 25, 2024
tags: [traverse, haskell, purescript, fp-ts, elm]
---

# Collecting successes and errors

## `traverse` ("`map` with effects")

Often we want to iterate through a collection of items, performing some effect for each item.
This means we want some function that looks like

```haskell
(a -> f b) -> t a -> result
```

where `a -> f b` is our effectful computation, `t a` is our collection (typically a list or an array of `a`s) and `result` could take a few different shapes depending on the requirements of our program, especially in the common case when the effect `f` encapsulates some notion of failure (like `TaskEither` in `fp-ts`, or anything with `ExceptT` in its stack in `Haskell`).

> [!TIP]
> The **tl;dr** is that you almost _always_ want (some version of) `traverse` in this situation—so often that ["the answer is always `traverse`" has become a meme](https://impurepics.com/posts/2020-10-03-always-traverse.html).

## A decision tree

<ul class="decision-tree">
  <li>
    <strong>Is there a notion of "partial" success (succeed with a warning)?</strong>
    <ul>
      <li><strong>Yes:</strong> Use `traverse` with `These`.</li>
      <li><strong>No:</strong> Continue.</li>
    </ul>
  </li>

  <li>
    <strong>If everything succeeds, do you want to collect _all_ successes?</strong>
    <ul>
      <li>
        <strong>Yes:</strong><ul>
          <li><strong>…and in case of failure I want to report just the first failure.</strong><p>Use `traverse` with `Either`/`ExceptT`.</p></li>
          <li><strong>…and in case of failure I want to report _all_ failures.</strong><p>Use `traverse` with `Validation`.</p></li>
        </ul></li>
      <li><strong>No:</strong> Continue.</li>
    </ul>
  </li>

  <li>
    <strong>Do you want just the _first_ success (if anything succeeds)?</strong>
    <ul>
      <li>
        <strong>Yes:</strong><ul>
          <li><strong>…and in case of failure I want to report just the last failure.</strong><p>Use `altAll`/`asum`/ `choice`/`oneOf` with `Either`/`ExceptT`.</p></li>
          <li><strong>…and in case of failure I want to report _all_ failures.</strong><p>Use `altAll`/`asum`/ `choice`/`oneOf` with `Validation`.</p></li>
        </ul>
      </li>
      <li><strong>No:</strong> Continue.</li>
    </ul>
  </li>

  <li>
    <strong>You want to just collect all successes and all failures?</strong>
    <p>Use `wilt` (or `partitionMap` if the only "effect" is error handling).</p>
  </li>
</ul>

##

`wilt` is from the `Witherable` type class in PureScript and `fp-ts`, and appears as [`mapEitherA`](https://hackage.haskell.org/package/filtrable-0.1.6.0/docs/Data-Filtrable.html#v:mapEitherA) in Haskell. Conceptually, "`partitionMap` with effects." It's a rarer bird than `traverse`.
