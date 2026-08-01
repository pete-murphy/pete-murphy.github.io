Often we want to iterate through a collection of items, performing some effect for each item.
This means we want some function that looks like

```haskell
(a -> f b) -> t a -> result
```

where `a -> f b` is our effectful computation, `t a` is our collection (typically a list or an array of `a`s) and `result` could take a few different shapes depending on the requirements of our program, especially in the common case when the effect `f` encapsulates some notion of failure (like `TaskEither` in `fp-ts`, or anything with `ExceptT` in its stack in `Haskell`).

> [!TIP]
> The **tl;dr** is that you almost _always_ want (some version of) `traverse` in this situation—so often that ["the answer is always `traverse`" has become a meme](https://impurepics.com/posts/2020-10-03-always-traverse.html).

## Are you going to branch on the result of the computation?

<details><summary>Yes, I want to distinguish between a single success and failure case at the end</summary>

### Is there a notion of _partial_ success (succeed with a warning)?

<details><summary>Yes</summary>

Use `traverse` with `These`.

If _everything_ at least partially succeeds, collect _all_ successes and _all_ warnings. Otherwise report _all_ failures and _all_ warnings.

```ts
import { task as T, taskThese as TTh, readonlyArray as RA } from "fp-ts";
import { flow, pipe } from "fp-ts/function";

const f = (str: string) => {
  const n = parseInt(str);
  if (isNaN(n)) {
    return TTh.left(`${str} is not a number`);
  }
  if (n < 4) {
    return TTh.right(n);
  }
  return TTh.both(`${n} is not less than 4`, n);
};

const allSuccesses = pipe(
  ["1", "2", "3"],
  RA.traverse(TTh.getApplicative(T.ApplyPar, RA.getSemigroup<string>()))(
    flow(f, TTh.mapLeft(RA.of)),
  ),
);

const someWarnings = pipe(
  ["1", "2", "3", "4", "5"],
  RA.traverse(TTh.getApplicative(T.ApplyPar, RA.getSemigroup<string>()))(
    flow(f, TTh.mapLeft(RA.of)),
  ),
);

const someFailures = pipe(
  ["1", "2", "c", "4", "e"],
  RA.traverse(TTh.getApplicative(T.ApplyPar, RA.getSemigroup<string>()))(
    flow(f, TTh.mapLeft(RA.of)),
  ),
);

allSuccesses().then(console.log);
// { _tag: 'Right', right: [ 1, 2, 3 ] }

someWarnings().then(console.log);
// {
//   _tag: 'Both',
//   left: [ '4 is not less than 4', '5 is not less than 4' ],
//   right: [ 1, 2, 3, 4, 5 ]
// }

someFailures().then(console.log);
// {
//   _tag: 'Left',
//   left: [ 'c is not a number', '4 is not less than 4', 'e is not a number' ]
// }
```

</details><!-- Partial? Yes -->

<details><summary>No</summary>

<details><summary>If <em>everything</em> succeeds I want to collect <em>all</em> successes.</summary>

<details><summary>Otherwise report <em>first</em> failure.</summary>

Use `traverse` with default `Either` `Applicative` instance.

```ts
import { taskEither as TE, readonlyArray as RA } from "fp-ts";
import { pipe } from "fp-ts/function";

const f = (str: string) => {
  const n = parseInt(str);
  if (isNaN(n)) {
    return TE.left(`${str} is not a number`);
  }
  return TE.right(n);
};

const allSuccesses = pipe(
  ["1", "2", "3", "4", "5"],
  RA.traverse(TE.ApplicativePar)(f),
);

const someFailures = pipe(
  ["1", "2", "c", "4", "e"],
  RA.traverse(TE.ApplicativePar)(f),
);

allSuccesses().then(console.log);
// { _tag: 'Right', right: [ 1, 2, 3, 4, 5 ] }

someFailures().then(console.log);
// { _tag: 'Left', left: 'c is not a number' }
```

</details><!-- Otherwise report _first_ failure. -->

<details><summary>Otherwise report <em>all</em> failures.</summary>

Use `traverse` with validation `Applicative` instance.

```ts
import { taskEither as TE, task as T, readonlyArray as RA } from "fp-ts";
import { flow, pipe } from "fp-ts/function";

const f = (str: string): TE.TaskEither<string, number> => {
  const n = parseInt(str);
  if (isNaN(n)) {
    return TE.left(`${str} is not a number`);
  }
  return TE.right(n);
};

const allSuccesses = pipe(
  ["1", "2", "3", "4", "5"],
  RA.traverse(
    TE.getApplicativeTaskValidation(
      T.ApplicativePar,
      RA.getSemigroup<string>(),
    ),
  )(flow(f, TE.mapError(RA.of))),
);

const someFailures = pipe(
  ["1", "2", "c", "4", "e"],
  RA.traverse(
    TE.getApplicativeTaskValidation(
      T.ApplicativePar,
      RA.getSemigroup<string>(),
    ),
  )(flow(f, TE.mapError(RA.of))),
);

allSuccesses().then(console.log);
// { _tag: 'Right', right: [ 1, 2, 3, 4, 5 ] }

someFailures().then(console.log);
// { _tag: 'Left', left: [ 'c is not a number', 'e is not a number' ] }
```

</details><!-- Otherwise report _all_ failures. -->

</details><!-- If _everything_ succeeds I want to ... -->

<details><summary>If <em>anything</em> succeeds I want to collect <em>first</em> success.</summary>

Both these examples are a bit awkward in `fp-ts` because it requires a `startWith` argument. In Haskell, there's a `Foldable1` class for non-empty foldable containers, which could use [`asum1`](https://hackage.haskell.org/package/semigroupoids-6.0.0.1/docs/Data-Semigroup-Foldable.html#v:asum1) to avoid needing a "starting" value.

<details><summary>Otherwise report <em>last</em> failure.</summary>

Use `altAll`

```ts
import { alt as Alt, taskEither as TE, readonlyArray as RA } from "fp-ts";
import { pipe } from "fp-ts/function";

const f = (str: string): TE.TaskEither<string, number> => {
  const n = parseInt(str);
  if (isNaN(n)) {
    return TE.left(`${str} is not a number`);
  }
  return TE.right(n);
};

const allSuccesses = pipe(
  ["2", "3", "4", "5"],
  RA.map(f),
  Alt.altAll(TE.Alt)(f("1")),
);

const startWithFailure = pipe(
  ["2", "c", "4", "e"],
  RA.map(f),
  Alt.altAll(TE.Alt)(f("a")),
);

const allFailures = pipe(["b", "c"], RA.map(f), Alt.altAll(TE.Alt)(f("a")));

allSuccesses().then(console.log);
// { _tag: 'Right', right: 1 }

startWithFailure().then(console.log);
// { _tag: 'Right', right: 2 }

allFailures().then(console.log);
// { _tag: 'Left', left: 'c is not a number' }
```

</details><!-- Otherwise report _last_ failure. -->

<details><summary>Otherwise report <em>all</em> failures.</summary>

Use `altAll` with validation `Alt` instance.

```ts
import { alt as Alt, taskEither as TE, readonlyArray as RA } from "fp-ts";
import { flow, pipe } from "fp-ts/function";

const f = (str: string): TE.TaskEither<string, number> => {
  const n = parseInt(str);
  if (isNaN(n)) {
    return TE.left(`${str} is not a number`);
  }
  return TE.right(n);
};

const g = flow(f, TE.mapError(RA.of));

const allSuccesses = pipe(
  ["2", "3", "4", "5"],
  RA.map(g),
  Alt.altAll(TE.getAltTaskValidation(RA.getSemigroup<string>()))(g("1")),
);

const startWithFailure = pipe(
  ["2", "c", "4", "e"],
  RA.map(g),
  Alt.altAll(TE.getAltTaskValidation(RA.getSemigroup<string>()))(g("a")),
);

const allFailures = pipe(
  ["b", "c"],
  RA.map(g),
  Alt.altAll(TE.getAltTaskValidation(RA.getSemigroup<string>()))(g("a")),
);

allSuccesses().then(console.log);
// { _tag: 'Right', right: 1 }

startWithFailure().then(console.log);
// { _tag: 'Right', right: 2 }

allFailures().then(console.log);
// {
//   _tag: 'Left',
//   left: [ 'a is not a number', 'b is not a number', 'c is not a number' ]
// }
```

</details><!-- Otherwise report _all_ failures. -->

</details><!-- If _anything_ succeeds I want to ... -->

</details><!-- Partial? No -->

<br>

</details><!-- Yes, I want to distinguish between a single success and failure case at the end -->

<details><summary>No, I just want to run the computation and collect all successes and all failures</summary>

Use `wilt`

```ts
import { taskEither as TE, task as T, readonlyArray as RA } from "fp-ts";
import { pipe } from "fp-ts/function";

const f = (str: string) => {
  const n = parseInt(str);
  if (isNaN(n)) {
    return TE.left(`${str} is not a number`);
  }
  return TE.right(n);
};

const allSuccesses = pipe(
  ["1", "2", "3", "4", "5"],
  RA.wilt(T.ApplicativePar)(f),
);

const someFailures = pipe(
  ["1", "2", "c", "4", "e"],
  RA.wilt(T.ApplicativePar)(f),
);

allSuccesses().then(console.log);
// { left: [], right: [ 1, 2, 3, 4, 5 ] }

someFailures().then(console.log);
// {
//   left: [ 'c is not a number', 'e is not a number' ],
//   right: [ 1, 2, 4 ]
// }
```

</details>
