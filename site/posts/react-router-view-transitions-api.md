---
author: Me
date: May 17, 2023
tags: [react, react-router, view transitions]
---

# React Router and View Transitions

## Container transform

Recently I wanted to implement something like what Material UI calls the [container transform pattern](https://m3.material.io/styles/motion/transitions/transition-patterns#b67cba74-6240-4663-a423-d537b6d21187). From the Material 3 docs:

> This pattern is used to seamlessly transform an element to show more detail, like a card expanding into a details page.
> [...]
> Persistent elements are used to seamlessly connect the start and end state of the transition.

Here's a simple example in a GIF

![An example from the Material UI documentation](../images/2023-05-21-container-transform.gif)

The pattern isn't unique to Material or Google, it's used for example in iOS when opening an app from the home screen.
I can't speak on how this is implemented on mobile, but on the web the implementation has historically been complicated by the variety of layout rules that need to be taken into account (Cassie Evans has an excellent [talk about the difficulties involved here](https://www.youtube.com/watch?v=POBxxUkvHi4)).

## FLIP

The solution that folks seem to have landed on for this is the [FLIP technique](https://aerotwist.com/blog/flip-your-animations/).
It's a bit of a magic trick that involves moving the element to its final state and then applying a `transform` so that we can efficiently animate _from_ the initial state, so that

> instead of animating "straight ahead" and potentially doing expensive calculations on every single frame we precalculate the animation dynamically and let it play out cheaply.

There are some libraries for implementing this, like [GSAP](https://greensock.com/docs/v3/Plugins/Flip/) or [Flipping.js](https://github.com/davidkpiano/flipping).
In React, `framer/motion` provides a high-level API for using this technique to achieve "shared layout animations" with its [`LayoutGroup`](https://www.framer.com/motion/layout-group/) component.

## View Transitions API

But there's also a new high-level API coming to the browser, in the [View Transitions API](https://developer.mozilla.org/en-US/docs/Web/API/View_Transitions_API).
This is still a [draft specification](https://drafts.csswg.org/css-view-transitions-1/) at time of writing, but it's landed in Chrome.
There's a nice [write-up by the Chrome team](https://developer.chrome.com/docs/web-platform/view-transitions/) that serves as a tutorial for using in a framework-less SPA.
Getting this to work in a React app where we don't normally manage DOM updates (especially for route transitions) is a bit trickier.

<!-- <image-loader
placeholder="../images/2023-05-21-demo/small.gif"
full-image="../images/2023-05-21-demo/large.gif"
/> -->

![A simple demo using React Router](../images/2023-05-21-demo/large.gif)

After a bit of trial-and-error I got a demo working using animated transitions between routes with React Router.
Here's the repo: [https://github.com/ptrfrncsmrph/react-view-transitions-api](https://github.com/ptrfrncsmrph/react-view-transitions-api).

## Some rough edges

`startViewTransition` takes a callback that _synchronously_ updates the DOM.
The only way I could figure to do so was using React Router's `useNavigate` and a button with a click handler instead of `Link` 😔.
We then need to wrap the call to `navigate` in `flushSync` to force the synchronous update.

```ts
document.startViewTransition(() => {
  ReactDOM.flushSync(() => {
    navigate(nextRoute);
  });
});
```

The React docs warn that `flushSync` should be used as a "last resort", and this API does seem to be at odds with the React mental model that doesn't normally care about _when_ DOM updates happen.

Another awkward bit is the need to toggle the `viewTransitionName`s for transitioning elements.

```ts
if (ref.current) {
  ref.current.style.viewTransitionName = "movie-image";
}
```

There needs to be exactly one element with the `"movie-image"` transition name at any given time, so [the recommendation](https://developer.chrome.com/docs/web-platform/view-transitions/#transitioning-elements-dont-need-to-be-the-same-dom-element) seems to be to _assign the tag name in the event handler_.

A nicer alternative might be to give unique names to each element, like `movie-image-${movie.id}` and then select pairs with `::view-transition-group(movie-image-*)` but that syntax doesn't exist and as far as I can tell the only way of achieving this currently would require creating just as many rules in the style sheet as there are pairs of elements you'd want to target.
I started [going down that road](https://github.com/ptrfrncsmrph/react-view-transitions-api/compare/main...dynamic-style-sheet-rules) but couldn't get it to work (the transitions _did_ apply but looked janky for reasons I couldn't understand).
_(I think this is also the approach one of the authors of the spec tried in this repo but seems like it had complications: [https://github.com/jakearchibald/wordle-analyzer/pull/19](https://github.com/jakearchibald/wordle-analyzer/pull/19))_

This unique name constraint makes "back navigation" transitions (from detail to list view) [messy](https://github.com/ptrfrncsmrph/react-view-transitions-api/commit/9c2a2775a34a2ea8e3a7e1ff90881cb4c8cf4e53#diff-26ad4b834941d9b19ebf9db8082bd202aaf72ea0ddea85f5a8a0cb3c729cc6f2).
The event handler doesn't have direct access to what we want to target as the `view-transition-new` element, so we need to find it in the DOM, assign the transition name, wait for the transition to complete, and finally remove the name (so it can be reassigned to the next item that gets clicked).
