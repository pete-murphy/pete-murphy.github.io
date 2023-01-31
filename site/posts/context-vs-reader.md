---
title: "Context vs Reader"
author: Me
date: Jan 21, 2022
tags: [react, context, reader]
---

https://twitter.com/ryanflorence/status/1616121628706103297?s=20&t=WSCLXkcCEXobbO3wLRYghw

> TypeScript is gonna bring back render props and kill hooks that access React context.

Could we do something like

```hs
type Component state props = ReaderT (Observable state) Effect (props -> JSX)
```
