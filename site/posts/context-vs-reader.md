---
author: Me
date: Jan 21, 2022
tags: [react, context, reader]
---

# Context vs Reader

Ryan Florence on [Twitter](https://twitter.com/ryanflorence/status/1616121628706103297?s=20&t=WSCLXkcCEXobbO3wLRYghw)

> TypeScript is gonna bring back render props and kill hooks that access React context.

Could we do something like

<Multicodeblock>

```purescript
type Component state props =
  ReaderT (Observable state) Effect (props -> JSX)
```

```typescript
type Component<State, Props> = ReaderIO<
  Observable<State>,
  (props: Props) => JSX
>;
```

```typescript
type Component<State, Props> = Effect<
  Observable<State>,
  never,
  (props: Props) => JSX
>;
```

</Multicodeblock>
