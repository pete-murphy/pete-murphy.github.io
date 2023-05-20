---
title: "Trying out the experimental View Transitions API in React"
htmlTitle: Trying out the experimental View Transitions API in React
displayTitle: Trying out the experimental View Transitions API in React
author: Me
date: May 17, 2023
tags: [react, react-router, view transitions]
---

# Trying out the experimental View Transitions API in React

Recently I wanted to implement Material UI's [container transform pattern](https://m3.material.io/styles/motion/transitions/transition-patterns#b67cba74-6240-4663-a423-d537b6d21187) in a side project app. From the Material 3 docs:

> This pattern is used to seamlessly transform an element to show more detail, like a card expanding into a details page.
>
> Persistent elements are used to seamlessly connect the start and end state of the transition. The most common persistent element is a container, which is a shape used to represent an enclosed area. It can also be an important element, like a hero image. Of all transition patterns, this one creates the strongest relationship between elements. It's also perceived to be the most expressive.

The pattern isn't unique to Material or Google, it's used for example in iOS when opening an app from the home screen.
Typically this is implemented using the ["FLIP" technique](https://aerotwist.com/blog/flip-your-animations/) and there are some high-level libraries for creating this effect (for example, in React I think the thing to use would be [`LayoutGroup`](https://www.framer.com/motion/layout-group/) from `framer/motion`.)

## View Transitions API

There's a new browser API for this though in the [View Transitions API](https://developer.mozilla.org/en-US/docs/Web/API/View_Transitions_API).
This is still a [draft specification](https://drafts.csswg.org/css-view-transitions-1/) at time of writing, but it's landed in Chrome.

## Working demo using transitions between routes

After a bit of trial-and-error I got a demo working using animated transitions between routes with React Router. Here's the repo: [https://github.com/ptrfrncsmrph/react-view-transitions-api](https://github.com/ptrfrncsmrph/react-view-transitions-api).

One rough bit that could be improved: I ended up using `useNavigate` and a button with a click handler (instead of `Link`) and wrapping the call to `navigate` in `flushSync`. This is necessary to force navigation change (and subsequent DOM updates) to happen within the scope of the callback passed to `startViewTransition`.

```ts
return document.startViewTransition(() => {
  return ReactDOM.flushSync(() => navigate_(nextRoute));
});
```

Not sure of an alternative here aside from pushing the `startViewTransition` call into the router library code.
