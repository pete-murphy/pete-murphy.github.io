---
author: Me
date: Apr 11, 2019
tags: [react, state reducer, reader]
---

![Kate Winslet in ***The Reader***
(2008)](https://miro.medium.com/v2/resize:fit:1400/0*dCaXfYO02-X22VR3.jpg){.full-width}

# State Reducer & the Reader Applicative

Kent C. Dodds [recently wrote
about](https://kentcdodds.com/blog/the-state-reducer-pattern-with-react-hooks) porting his "state
reducer" pattern to the new React hooks API. I'm going to assume some
understanding of that blog post (or [this
video](https://youtu.be/AiJ8tRRH0f8) where he presents the same concept) going forward, but
in short, state reducer is a way of implementing the _inversion of
control_ principle in a component library by allowing a user of a
component to pass in a `reducer` function that
will be called by the library code on each state update. This enables
the user to make some custom changes to suit whatever specialized use
case, and frees the library author from covering every possible use case
(also avoiding the bloated code that would result from doing so).

Looking through the implementation of how Kent implements this pattern,
I was reminded of another pattern from functional programming that I was
learning at the same time called Reader. I wanted to explore how you
might implement the former in terms of the latter, not because I think
it offers any improvement necessarily, I just find it helpful to draw
parallels between patterns to help gain a deeper understanding.

## State Reducer Pattern

Let's pick up where Kent's blog post leaves off: he's got a working
(though he admits "contrived") `useToggle` hook
that can be used to create a `Toggle` component.
The `useToggle` hook takes as parameter an
object with a `reducer` property, which is a
function with the standard reducer type signature of
`(state, action)` to `state` with an important embellishment: the action has a `changes` property attached to it. This `changes` property is populated by the library code, when it runs its own
`toggleReducer` function. So `toggleReducer` has the signature of `(state, action)` to `changes`. This is a little
convoluted to think through without looking at an example and the blog
post does a good job of building up to this point. Here's the final code
of how this works in a simple example (also [here in a
CodeSandbox](https://codesandbox.io/s/9j0pkq30lo)):

```js
import React from "react";
import ReactDOM from "react-dom";
import Switch from "./switch";

const actionTypes = {
  toggle: "TOGGLE",
  on: "ON",
  off: "OFF",
};

function toggleReducer(state, action) {
  switch (action.type) {
    case actionTypes.toggle: {
      return { on: !state.on };
    }
    case actionTypes.on: {
      return { on: true };
    }
    case actionTypes.off: {
      return { on: false };
    }
    default: {
      throw new Error(`Unhandled type: ${action.type}`);
    }
  }
}

function useToggle({ reducer = toggleReducer } = {}) {
  const [{ on }, dispatch] = React.useReducer(reducer, { on: false });

  const toggle = () => dispatch({ type: actionTypes.toggle });
  const setOn = () => dispatch({ type: actionTypes.on });
  const setOff = () => dispatch({ type: actionTypes.off });

  return { on, toggle, setOn, setOff };
}

// export {useToggle, actionTypes, toggleReducer, actionTypes}

function Toggle() {
  const [clicksSinceReset, setClicksSinceReset] = React.useState(0);
  const tooManyClicks = clicksSinceReset >= 4;

  const { on, toggle, setOn, setOff } = useToggle({
    reducer(currentState, action) {
      const changes = toggleReducer(currentState, action);
      if (tooManyClicks && action.type === actionTypes.toggle) {
        // other changes are fine, but on needs to be unchanged
        return { ...changes, on: currentState.on };
      } else {
        // the changes are fine
        return changes;
      }
    },
  });

  return (
    <div>
      <button onClick={setOff}>Switch Off</button>
      <button onClick={setOn}>Switch On</button>
      <Switch
        onClick={() => {
          toggle();
          setClicksSinceReset((count) => count + 1);
        }}
        on={on}
      />
      {tooManyClicks ? (
        <button onClick={() => setClicksSinceReset(0)}>Reset</button>
      ) : null}
    </div>
  );
}

function App() {
  return <Toggle />;
}

ReactDOM.render(<App />, document.getElementById("root"));
```

I found this a bit tricky to follow at first but the gist of it is that
as a user of the `useToggle` hook, I can add in
some custom logic that could modify how the component's internal state
gets updated.

## Introducing Reader

At this point, I'd like to introduce you to Reader, specifically the
Reader applicative instance, which helped me get a better grasp of the
state reducer pattern. Reader is just a function whose input is fixed to
some type, which might not seem very special, but is a very useful
pattern in the functional programming paradigm where you'll often see it
referred to as the Reader Monad. I'm not going to cover the "M" word
here; all we need is the weaker, humbler applicative instance for
Reader. To get an applicative instance for some type we need to
implement a couple functions that work for that type.

```haskell
pure :: Applicative f => a -> f a
lift2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
```

Let's narrow our focus to `lift2` (sometimes
called `liftA2`) for now. We just want to write
a `lift2` that works for Reader, which we said
was a function whose input type is fixed, i.e., `r ->` where `r` is the input type and so
`r ->` might be pronounced "_a function from_
`r` _to ..."_ So then we can replace all the
`f`s in the above signature with `r ->`:

```haskell
lift2 :: (a -> b -> c) -> (r -> a) -> (r -> b) -> (r -> c)
```

The function arrow `->` associates to the right,
so that last pair of parentheses is redundant.

```haskell
lift2 :: (a -> b -> c) -> (r -> a) -> (r -> b) -> r -> c
```

If you're just barely comfortable with higher-order functions, this is a
bit daunting. The best I can offer for making sense of that signature is
to try to think of a way to read this out loud, which might go something
like: "**_lift2_** is a function that has the type **_f_** to **_g_** to
**_h_** to **_r_** to **_c_**, where: **_f_** is a function of type
**_a_** to **_b_** to **_c_**; **_g_** is a function of type **_r_** to
**_a_**; and **_h_** is a function of type **_r_** to **_b_**." If this
seems pretty heavy, it is, but don't fret and hopefully the following
implementation and explanations will fill in the gaps. What it amounts
to in JavaScript is:

```javascript
const lift2 =
  (abc) =>
  (ra) =>
  (rb) =>
  (...r) => {
    const a = ra(...r);
    const b = rb(...r);
    const c = abc(a)(b);
    return c;
  };
```

`abc` (the function from `a -> b -> c`) is a way of combining the results of applying the same
argument(s) (we're representing the fact that JavaScript functions can
take an arbitrary number of arguments by spreading `...r`) to two functions: `ra` and `rb`. **In short, all we're doing is running two
functions with the same argument and then doing something with the
results.** This sounds a lot like what was going on with the state
reducer pattern (with a few modifications). In other words, we could
refactor the `useToggle` hook to use this
`lift2`, but first we want to refactor
`reducer` and `toggleReducer` to take the same arguments (these will be our `ra` and `rb` functions). Let's just have
them take the regular Redux-style reducer argument of
`(state, action)`, (with no extra `changes` property attached to action).

We can refactor `toggleReducer` so now it just
returns a new state:

```javascript
function toggleReducer(state, action) {
  switch (action.type) {
    case useToggle.types.toggle: {
      return { ...state, on: !state.on };
    }
    case useToggle.types.on: {
      return { ...state, on: true };
    }
    case useToggle.types.off: {
      return { ...state, on: false };
    }
    default: {
      return state;
    }
  }
}
```

and then we can refactor the `reducer` function
coming from the user so that it just adds whatever changes or overrides
the user wants to add:

```javascript
const { on, toggle, setOn, setOff } = useToggle({
  reducer(currentState, action) {
    if (tooManyClicks && action.type === useToggle.types.toggle) {
      return { on: currentState.one };
    }
  },
});
```

Finally, the call to `useReducer` inside of
`useToggle` will look like:

```javascript
function useToggle({ reducer = (_s, _a) => {} } = {}) {
  const merge = (newState) => (changes) => ({ ...newState, ...changes });
  const [{ on }, dispatch] = React.useReducer(
    lift2(merge)(toggleReducer)(reducer),
    { on: false }
  );
}
```

Running the library-provided `toggleReducer`
with `(state, action)` gives us back
`newState`, and running the user-provided
`reducer` with `(state, action)` gives us back some custom `changes`. We
have a way of combining `newState` and
`changes` with the `merge` function. Finally, `lift2` handles the the
logic of threading the `(state, action)` pair
through the two reducer functions and running `merge` on the results.

This sounds like a lot of theory, _but does it work?_ Yes, it appears
to: [here's both components side-by-side in a
CodeSandbox](https://codesandbox.io/s/6zx5qrvvrr).

## What did we learn here?

So we saw how the state reducer pattern could be rewritten using the
Reader applicative pattern. I would like to expand on what makes Reader
a worthwhile abstraction here, but I'm still learning and exploring this
stuff myself, and this post is long enough as is. I _think_ another
thing we could do is leverage the applicative instance a bit more by
using `sequence` (another function that depends
on applicative) to merge a collection of overrides --- if, for instance,
another user wanted to extend the first user's extension of the library
component. I'm not sure how all that would play out, so I think that's
all for now. Hopefully you've found something useful or interesting or
thought-provoking here.

![Tip of the iceberg in **Titanic**
(1997)](https://miro.medium.com/v2/resize:fit:1400/0*ZUmbZdEScbAGgGid.jpg){.full-width}

## Diving deeper

This is just the tip of the iceberg for exploring these concepts:
applicative is a much more general abstraction of types that implement
an interface, and Reader has more use cases than what's covered here. If
you're interested in learning more, I'd highly recommend the following
resources:

- [_Mostly Adequate Guide to Functional Programming in
  JS_](https://github.com/MostlyAdequate/mostly-adequate-guide) (free
  e-book)
- [_Fantas, Eel, and
  Specification_](http://www.tomharding.me/fantasy-land/) (series of blog
  posts)
- [[_Monad-a-Day: Reader_](https://vimeo.com/105300347) (video)]{#49e6}
- [_Fantasy Land_](https://github.com/fantasyland/fantasy-land) and [_Static
  Land_](https://github.com/rpominov/static-land) (specifications for how
  these types/interfaces can be implemented in JavaScript)
