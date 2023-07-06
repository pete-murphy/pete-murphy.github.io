---
author: Me
date: Jul 3, 2018
tags: []
---

# Better Readability with Function Composition

<div class="tldr">**TL;DR** Function composition is an early win on the FP learning curve, and the best presentation of this material I've found is in the [_Prof Frisby_ book](https://mostly-adequate.gitbooks.io/mostly-adequate-guide/), so I'd point to that as a primary resource. If interested in jumping ahead to some composition exercises (and slightly easier than those in the book), check out either [Kyle Shevlin's](https://github.com/kyleshevlin/fp-composition-exercises) or [my own repo](https://gitlab.com/ccfp/intro-to-fp) (based on his).
</div>

Last week we had our first meetup, and I thought I would sum up what was covered for those that couldn't make it. The goal was to cover the concept of _function composition_: in short, composing small, simple functions into bigger, more complex ones. I hope this post is read as a supplement or companion piece to other more in-depth treatments of the subject, as I may gloss over or neglect to mention a lot of concepts that help contribute to a better understanding of function composition, namely: _purity_, _immutability_, _higher-order functions_, _currying_ and _partial application_. For a quick overview of some of those concepts (in JavaScript), I recommend [this talk](https://www.youtube.com/watch?v=e-5obm1G_FY) by Anjana Vakil, or [this one](https://www.youtube.com/watch?v=-4QNj7TJjgo) by Kyle Shevlin. Khan Academy also has a helpful [section on composing functions](https://www.khanacademy.org/math/algebra2/manipulating-functions/function-composition/v/function-composition) with diagrams and probably better, more in-depth explanations than you'll find below.

## Taking a page out of your high school algebra book

You may remember function composition from high school math:

$$
g(f(x)) = (g \circ f)(x)
$$

On the left hand side of the equation, we're _applying_ the function $f$ to $x$ (or in programming parlance, calling $f$ with $x$ as argument) and then applying $g$ to the result. On the right hand side we are introducing the composition operator (little circle) and saying that the composed function $g \circ f$ applied to $x$ yields the same result.

It may not be obvious how that concept could be useful, but consider a case where the nesting of parentheses (or function calls, however you want to think of it) gets a bit out of hand:

$$
j(i(h(g(f(x))))) = (j \circ i \circ h \circ g \circ f)(x)
$$

In this case, composition allows us to think about a single composed function ($j \circ i \circ h \circ g \circ f$) that will be applied to an $x$. Contrast that with the left side where we have to first call $f(x)$, and then pass the result to $g$, then pass that result to $h$ and so on.

## Real world application

Suppose you want to write a function that converts a roof pitch[^1] to degrees. E.g., for the input _string_ `"5:12"`, we want to return the _number_ `22.6`. A first attempt might go like so:

```javascript
const pitchToDegrees = (str) =>
  Math.round(
    ((Math.atan(+str.split(/[:/]/)[0] / +str.split(/[:/]/)[1]) * 180) /
      Math.PI) *
      10
  ) / 10;
```

Which works alright, but it's not so easy to follow how that initial string _flows_ through each step of the function to produce the final result. You could try to make it more explicit by assigning each step to a variable:

```javascript
const pitchToDegrees = (str) => {
  const splitStr = str.split(/[:/]/);
  const splitNums = splitStr.map(Number);
  const radians = Math.atan(splitNums[0] / splitNums[1]);
  const degrees = (radians * 180) / Math.PI;
  const rounded = Math.round(degrees * 10) / 10;
  return rounded;
};
```

But that gets pretty verbose, and since you're just passing each result into the next expression, it might be more natural to read this as a composed function:

```javascript
const pitchToDegree = compose(
  roundTo(1),
  radiansToDegrees,
  ([rise, run]) => Math.atan(rise / run),
  map(parseFloat),
  split(/[:/]/)
);
```

I think this strikes a balance between terseness and easy readability (once you get used to reading composed functions from right-to-left, bottom-to-top). It's also [been pointed out](https://www.youtube.com/watch?v=SfWR3dKnFIo) that this style of programming lends to more reliable, safer code, as it is harder to hack (in the sense of throwing things together in an undisciplined or naive manner). Every returned value has to necessarily flow into the next function; you really have to go out of your way to do any side effect-y things.[^3]

It does require a little bit of setup beforehand with some helper functions:

```javascript
const map = (fn) => (arr) => arr.map(fn);
const split = (regEx) => (str) => str.split(regEx);
const roundTo = (digits) => (num) =>
  Math.round(num * 10 ** digits) / 10 ** digits;
const radiansToDegrees = (rads) => (rads * 180) / Math.PI;
```

That might seem like a lot of work defining all these small, super simple functions, but that's kind of the name of the game with functional programming, and the idea is that it pays off because all these little functions are easy to test and are usually very re-usable[^2]. If you don't feel like rolling your own tiny functions by hand, it's easy enough to pull them in from a handy library like [ramda](https://ramdajs.com) or [lodash/fp](https://github.com/lodash/lodash/wiki/FP-Guide), which will have most of what you would need and more.
We haven't covered the one function that makes it possible to glue all the little functions together, a simple definition for which might look like:

```javascript
const compose =
  (...fns) =>
  (x) =>
    fns.reduceRight((acc, fn) => fn(acc), x);
```

This definition can look pretty gnarly if you're not used to looking at the array `reduce` method and the function that it takes as an argument, or the rest parameters syntax in JavaScript---but I wouldn't worry if it looks alien. If it helps, you can think of it as the more generalized version of what we're doing here:

```javascript
const compose2 = (g, f) => x => g(f(x))
const compose3 = (h, g, f) => x => h(g(f(x)))
const compose4 = (i, h, g, f) => x => i(h(g(f(x))))
...
```

Or you can just start using it and worry about the details behind how it's implemented later, which tends to be how I learn. If you're interested in learning more, this material is covered in the first five chapters of [_Professor Frisby Mostly Adequate Guide_](https://mostly-adequate.gitbooks.io/mostly-adequate-guide/), or if you want to fast forward to trying out some composition exercises I have a [repo of some here](https://gitlab.com/ccfp/intro-to-fp).

[^1]: The steepness of the roof of a house is often expressed (at least in the US) in terms of common _pitches_ such as 5:12, 6:12, 8:12, 10:12, where the first number is the rise in inches over a 12 inch span (so, 5:12 is read as "5 over 12", or just a "5 pitch"). Roofers and architects alike think of the angle of a roof in these terms.
[^2]: Okay, maybe `radiansToDegrees` isn't super re-usable.
[^3]: Speaking of which, a handy (and impure) function that I often rely on to debug when writing functions in this way is

```javascript
const trace = (msg) => (x) => {
  console.log(msg, x);
  return x;
};
```

Just drop it in in the middle of a composed function and you can have a peek at what's being passed through at that point.
