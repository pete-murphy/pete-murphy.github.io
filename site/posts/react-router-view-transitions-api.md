---
title: Experimenting with View Transitions API in React
htmlTitle: Experimenting with View Transitions API in React
displayTitle: Experimenting with View Transitions API in React
author: Me
date: May 17, 2023
tags: [react, react-router, view transitions]
---

Recently I wanted to implement something like what Material UI calls the [container transform pattern](https://m3.material.io/styles/motion/transitions/transition-patterns#b67cba74-6240-4663-a423-d537b6d21187). From the Material 3 docs:

> This pattern is used to seamlessly transform an element to show more detail, like a card expanding into a details page.
> [...]
> Persistent elements are used to seamlessly connect the start and end state of the transition.

Here's a simple example in a GIF

![](../images/2023-05-21-container-transform.gif)

The pattern isn't unique to Material or Google, it's used for example in iOS when opening an app from the home screen.
Can't really speak on how easy this is to implement on mobile, but on the web the implementation has historically been complicated by the variety of layout rules that need to be taken into account (Cassie Evans has an excellent [talk about the difficulties involved here](https://www.youtube.com/watch?v=POBxxUkvHi4).)

The solution that folks seem to have landed on for this is the [FLIP technique](https://aerotwist.com/blog/flip-your-animations/).
It's a magic trick that's made a bit of sense of when you consider that (on the web) it's most natural to specify the beginning and end states of an animation, and the start time (usually at the moment of some user event).
There are some high-level libraries for creating this effect (for example, in React I think the thing to use would be [`LayoutGroup`](https://www.framer.com/motion/layout-group/) from `framer/motion`.)

## View Transitions API

There's a new browser API for this though in the [View Transitions API](https://developer.mozilla.org/en-US/docs/Web/API/View_Transitions_API).
This is still a [draft specification](https://drafts.csswg.org/css-view-transitions-1/) at time of writing, but it's landed in Chrome.

### Minimal demo using transitions between routes

![](https://github.com/ptrfrncsmrph/react-view-transitions-api/assets/26548438/ac3b1eb0-b2af-49ca-a315-79346f8cb7ab)

After a bit of trial-and-error I got a demo working using animated transitions between routes with React Router. Here's the repo: [https://github.com/ptrfrncsmrph/react-view-transitions-api](https://github.com/ptrfrncsmrph/react-view-transitions-api).

One rough bit: I ended up using `useNavigate` and a button with a click handler (instead of `Link`) and wrapping the call to `navigate` in `flushSync`. This is necessary to force navigation change (and subsequent DOM updates) to happen within the scope of the callback passed to `startViewTransition`.

There ends up being this nesting of "effects"

```ts
document.startViewTransition(() => {
  ReactDOM.flushSync(() => {
    navigate_(nextRoute);
  });
});
```

The React docs warn that `flushSync` should be used as a "last resort".
Not sure of an alternative here, at the least maybe the `startViewTransition` call could be abstracted away into the router library code.

Another awkward bit

```ts
event.currentTarget.firstElementChild.style.viewTransitionName = "image-test";
```

The more "idiomatic" thing in React would be to move each of the movie list items into a component that can call `useRef` to do that assignment, but really the underlying issue is that there needs to be exactly one element with the `"image-test"` transition name at any given time, so [the solution](https://developer.chrome.com/docs/web-platform/view-transitions/#transitioning-elements-dont-need-to-be-the-same-dom-element) seems to be _assign the tag name on user interaction (click)_.
I wanted to give unique names to each element (like

```typescript
viewTransitionName: `image-test-${movie.id}`;
```

), but that would have required creating just as many rules in the style sheet as there are elements (at the time there doesn't seem to be a way of selecting `::view-transition-group(image-test-*)` or similar, though that would be ideal.)
