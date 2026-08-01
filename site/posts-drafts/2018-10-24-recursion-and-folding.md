---
author: Me
date: Oct 24, 2018
tags: []
---

# Recursion and Folding in JavaScript

After much delay, I am finally getting around to writing a follow-up to our first in-person meetup! This time we were talking about _recursion_ and a particular pattern that arises when applying recursion on JavaScript arrays, which I'll call "folding" for now. There's plenty to talk about with recursion, so for this post I'll just concentrate on folding, but keep in mind that there are plenty of other use cases for recursion. You can look over the Git repo for the meetup [here](https://github.com/ccfp/recursion-and-folding-in-js)---there are two branches: `master` was our starting point and solutions we worked out were pushed on to a separate branch, `solved`.

## A recursive `sum`

Let's suppose we wanted to write a recursive `sum` function that works on JavaScript arrays. So `sum([1, 3, 8])` should equal `1 + 3 + 8`, or `12`. If we want to think of this as a recursive function we want to re-frame it as a _function that calls itself_ (we'll take this as a minimal definition of recursion for now). OK, so instead of saying (in pseudo-code):

```javascript
sum([1, 3, 8]) = 1 + 3 + 8
```

we could say

```javascript
sum([1, 3, 8]) = 1 + sum([3, 8])
```

Now it's starting to look recursive! But how do we translate that into a function definition? Let's try:

```javascript
const sum = (arr) => {
  const [num, ...nums] = arr;
  return num + sum(nums);
};
```

Here we're using array destructuring to get the _head_, or first element (`num`), and _tail_, or everything but the first element (`nums`), of our array. We're adding the `num` to the result of calling `sum` on the `nums` (kind of like what we were doing with `sum([1, 3, 8]) = 1 + sum([3, 8])`). If we try to call this function (you can just copy paste into the console of your browser to try it out) we get the following error:

```javascript
sum([1, 3, 8]);
// -> Uncaught RangeError: Maximum call stack size exceeded
```

This is a common error that arises when recursion goes wrong: we didn't tell the function to _stop calling itself_ at any point, and so it'll keep calling `sum` on the tail (`nums`), even when `nums` is an empty array.

To see what that means, let's add a `console.log` in there and peek at the results.

```javascript
const sum = (arr) => {
  const [num, ...nums] = arr;
  console.log({ arr, num, nums });
  return num + sum(nums);
};
```

Now if we call it

```javascript
sum([1, 3, 8]);
```

we'll get this printed to the console:

```javascript
{ arr: [ 1, 3, 8 ], num: 1, nums: [ 3, 8 ] }
{ arr: [ 3, 8 ], num: 3, nums: [ 8 ] }
{ arr: [ 8 ], num: 8, nums: [] }
{ arr: [], num: undefined, nums: [] }
{ arr: [], num: undefined, nums: [] }
{ arr: [], num: undefined, nums: [] }
{ arr: [], num: undefined, nums: [] }
{ arr: [], num: undefined, nums: [] }
{ arr: [], num: undefined, nums: [] }
... (and so on, ad nauseam, until the stack overfloweth)
```

We probably wanted to stop once we got down to the empty array, but as you can see, our function keeps destructuring and gets a new empty array and calls itself with that, and so on. We need some way of putting a stop to this!

## The base case---an "off switch" for the recursive call

Let's introduce a _base case_, which is just a condition that, once met, will give us a way of opting out of the recursive call.

```javascript
const sum = (arr) => {
  if (arr.length === 0) return;
  const [num, ...nums] = arr;
  return num + sum(nums);
};
```

This should stop calling `sum` once we've reached the empty array `[]` (in other words, when `arr.length === 0`) and at that point just `return`---sounds good, right? Let's give it a go:

```javascript
sum([1, 3, 8]);
// -> NaN
```

Bummer. Well, at least it's not giving us a stack overflow, so we must be getting closer. Let's reason through how this is evaluating:

```javascript
sum([1, 3, 8]);
// which we said is equal to
1 + sum([3, 8]);
// which is equal to
1 + 3 + sum([8]);
// which is equal to
1 + 3 + 8 + sum([]);
```

and `sum([])` is our base case, and we said that was equal to... well we just said we would `return` at that point, but anytime you `return` without a value in JavaScript, you're implicitly returning `undefined`. So ultimately this is evaluating to

```javascript
1 + 3 + 8 + undefined;
```

which is indeed _not a number_ (`NaN`). If we want to return our sum, we need to replace `undefined` with a value that will have no effect on our summation (or addition in general), and that would be `0`:

```javascript
const sum = (arr) => {
  if (arr.length === 0) return 0;
  const [num, ...nums] = arr;
  return num + sum(nums);
};
```

And now it works:

```javascript
sum([1, 3, 8]);
// -> 12
```

Yay! 🎉

## A lesson from an iterative `sum`

Even though we got our recursive `sum` function working, there's something that doesn't feel right about returning `0` on the last call---you'd think you'd be returning the _result_ of your summation. Like, if I were to define `sum` iteratively, I might come up with

```javascript
const sum = (arr) => {
  let summedValue = 0;
  for (const num of arr) {
    summedValue += num;
  }
  return summedValue;
};
```

And this feels more intuitive: we're starting with `0`, adding each `num` as we go, and at the end we're returning the total (`summedValue`). But, we can't really do the same thing in our recursive function. If we were to declare `let summedValue = 0` in our function body, it would just get reset to `0` on each call. What we _can_ do is pass it in as a second argument, and initialize it at `0` using ES6 default parameter syntax:

```javascript
const sum = (arr, summedValue = 0) => {
  ...
}
```

And then on the next recursive call we add our `num` to the `summedValue`:

```javascript
return sum(nums, summedValue + num);
```

and in our base case, we just return the `summedValue`:

```javascript
if (arr.length === 0) return summedValue;
```

Altogether it looks like:

```javascript
const sum = (arr, summedValue = 0) => {
  const [num, ...nums] = arr;
  if (arr.length === 0) return summedValue;
  return sum(nums, summedValue + num);
};
```

## Paring it down

If we try to refactor this to something more concise, we might get:

```javascript
const sum = ([num, ...nums], summedValue = 0) =>
  num === undefined ? summedValue : sum(nums, summedValue + num);
```

If you don't trust me that this still works, give it the ol' copy-paste into the console.[^1] Now let's see if we can use this as a template to define other array functions. _Errr..._ I'm actually just going to go ahead and say that you _can in fact_ use this as a template for defining other array functions. For example, if I just change a couple things, I've got `reverse`:

```javascript
const reverse = ([num, ...nums], summedValue = []) =>
  num === undefined ? summedValue : reverse(nums, [num, ...summedValue]);

reverse([1, 3, 8]);
// -> [8, 3, 1]
```

Or we can make the higher-order function `any`:

```javascript
const any =
  (predFn) =>
  ([num, ...nums], summedValue = false) =>
    num === undefined
      ? summedValue
      : any(predFn)(nums, predFn(num) || summedValue);

any(isOdd)([1, 3, 8]);
// -> true
```

Of course, we're not limited to talking about `num`s here, and it doesn't make sense to say that a `summedValue` is the thing we're returning, so I'm going to do something that you may find even more offensive (bear with me 🐻) and rename these variables to the more generic `x` and `acc` (short for _accumulated value_ or _accumulator_):

```javascript
const sum = ([x, ...xs], acc = 0) => (x === undefined ? acc : sum(xs, acc + x));

const reverse = ([x, ...xs], acc = []) =>
  x === undefined ? acc : reverse(xs, [x, ...acc]);

const any =
  (predFn) =>
  ([x, ...xs], acc = false) =>
    x === undefined ? acc : any(predFn)(xs, predFn(x) || acc);
```

Refactoring things to this minimal representation allows us to more easily see a pattern: notice that in each of these functions, we have some operation that combines our `x` with `acc` somehow---whether addition, concatenation or logical disjunction (the `||` operator)---and we're initializing the `acc` variable with some value that is _neutral_ in regards to that operation---`0` for `+`, `[]` for concatenation, and `false` for `||`.[^2] Aside from those two unique elements, everything else is repetition that we should be able to factor out into its own function. Let's call it `fold`:

```javascript
const fold = ([x, ...xs], acc, foldingFn) =>
  x === undefined ? acc : fold(xs, foldingFn(acc, x), foldingFn);
```

This may seem like a lot to behold, but all that I've done is taken our template and changed it so that `acc` no longer has a default value (we'll have to leave that to the caller to pass in, since it depends on what "combining" operation they are doing) and we're also passing in an extra parameter: a `foldingFn`, or folding function, for lack of a better term---this is the same thing as the "combining" operation (addition/concatenation/disjunction) expressed as a function that takes `acc` and `x` (current value). Now I can re-write `sum`, `reverse`, and `any` in terms of `fold`:

```javascript
const sum = (xs) => fold(xs, 0, (acc, x) => x + acc);

const reverse = (xs) => fold(xs, [], (acc, x) => [x, ...acc]);

const any = (predFn) => (xs) => fold(xs, false, (acc, x) => predFn(x) || acc);
```

This works, but we can do a little better by arranging `fold` so it can be used _point free_:

```javascript
const fold =
  (foldingFn, acc) =>
  ([x, ...xs]) =>
    x === undefined ? acc : fold(foldingFn, foldingFn(acc, x))(xs);
```

This lets us clean up our other functions a little bit:

```javascript
const sum = fold((acc, x) => x + acc, 0);

const reverse = fold((acc, x) => [x, ...acc], []);

const any = (predFn) => fold((acc, x) => predFn(x) || acc, false);
```

## `fold` unmasked

Turns out `fold` is something we already know and love in JavaScript---it's just `reduce`![^3]

```javascript
const reduce =
  (reducer, acc) =>
  ([x, ...xs]) =>
    x === undefined ? acc : reduce(reducer, reducer(acc, x))(xs);
```

Granted it's a standalone version, not a method on the `Array` prototype, but it works just the same. And we can define other array functions than just `sum`, `reverse` and `any` with it; in fact, **any** array transformation can be defined in terms of `reduce` (that's an open challenge!), which is pretty powerful stuff.

## Big words to google

_Whew_, this has been a doozy of a post, and I've pretty much exhausted my knowledge about this stuff so I can't say much more with certainty, but if you're interested in learning more I'd say it might be worthwhile looking into how the `reduce`/`fold` function is generalized by the concept of a _catamorphism_ (which is one of many _recursion schemes_).[^4] Of course `reduce` doesn't have to be defined recursively, we could write it iteratively, it's just an implementation detail,[^5] but it gives us a way of abstracting out this very powerful pattern. And I _think_ the power of recursion schemes in general is that you can abstract out such patterns not just for arrays but for any data structure (like linked lists, trees, maps, etc.)---but, not sure yet. To be continued... 🕵️‍♂️

[^1]: Notice that I'm doing the destructuring straightaway as part of the function call, which means I've lost my way to reference the whole array (`arr` is nowhere to be found) and so I'm checking for the head (`num`) to be `undefined`, which strictly speaking is not the same as the array having length `0`---if you're working with a sparsely-defined array (like, `[1, , 8]` or `[1, undefined, 8]`) this won't work---but for our purposes we'll pretend they're equivalent. 🤫
[^2]: If I add `0` to any number, I'll just get that number back. If I concatenate `[]` to any array, I'll just get that array back. If I say `x || false` where `x` is some boolean, I'll just get `x` back.
[^3]: Though it _is_ often called `fold` in other languages.

<div class="erratum">**Erratum:** It's been pointed out to me that `fold` does _not_ work exactly like `reduce`, in that the `initialValue` is optional in the latter. A more faithful recursive version might be written:
```javascript
const fold = (foldingFn, acc) => ([x, ...xs]) =>
  acc !== undefined
        ? x === undefined
          ? acc
          : fold(foldingFn, foldingFn(acc, x))(xs)
        : fold(foldingFn, x)(xs)
```
</div>

[^4]: A _catamorphism_ is described as a function that "destruct\[s\] a list" or other data structure (in other words, a function _from_ a structure containing type $A$ _to_ a value of type $B$). See ["Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire"](https://maartenfokkinga.github.io/utwente/mmf91m.pdf).
[^5]: Hope you don't feel cheated by my mentioning that in the last paragraph!
