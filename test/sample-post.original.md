---
title: "Stateful for loops in Rust & Haskell"
author: Me
date: Jan 1, 2019
tags: [slick, site, data abstraction, haskell, typescript]
description: My first blog post using slick
---

# Stateful `for` loops in Rust & Haskell

**Advent of Code spoilers herein**

Let's look at some `for` loops, shall we?!

Day five of this year's Advent of Code this year provides a nice occasion for doing some stateful computation. We were given:

- an initial state of some stacks of crates
- a list of instructions for moving crates from one stack to another

In Haskell I ended up using the `State` ~~monad~~ applicative functor, and I am compelled to write down why, because it was not that long ago that `State` was completely baffling to me.

```haskell
newtype State s a = State { runState :: s -> (a, s) }
```

How does this really model "state"?—how can we use this the same way that we use a global mutable reference in imperative-style programming.
Let's get some imports out of the way first.

<Multicodeblock>

```haskell
import Control.Monad.State (modify)
import Data.Foldable (for_)
import
```

```rust

```

```haskell
-- interface, abstract type: A typeclass
class ExampleClass a where
  create :: String -> a
  read1 :: a -> String
```

```fsharp
type ExampleInterface<'a> =
    abstract member create : string -> 'a
    abstract member read   : 'a -> string
```

</Multicodeblock>

### Implementation

<Multicodeblock>

```swift
// implementation: A struct (or a class)
public struct Example {
  let s: string

  public static func create(_ s: string) -> Example {
    return Example(s: s)
  }

  public func read() -> String {
    return self.s
  }
}
```

```typescript
// implementation: A function (for static members)
//                 A class (for instance ones)
function createExample(ctor: ExampleConstructor, s: string): ExampleInterface {
  return new ctor(s);
}

class Example implements ExampleInterface {
  s: string;
  constructor(s: string) {
    this.s = s;
  }
  read(): string {
    return this.s;
  }
}
```

```haskell
-- implementation: A typeclass instance
instance ExampleClass String where
  create = id
  read1 = id
```

```fsharp
type Example () =
    interface ExampleInterface<string> with
        member _.create s = s
        member _.read s = s
```

</Multicodeblock>

### Client

<Multicodeblock>

```swift
func client<E: ExampleInterface>() {
  let ex = E.create("hello");
  println(ex.read());
}
```

```typescript
// client
// it's a bit hard to write down a generic function for this in TypeScript
// because there are two separate interfaces
// ...
const ex = createExample(Example, "hello");
console.log(ex.read());
```

```ocaml
(* a client is a functor *)
module Client(E: Example_intf) = struct
  let ex = E.create "hello" in
  printf "%s\n" (E.read ex)
end

(* we can make clients first-class with first-class modules *)
let client (module E: Example_intf) =
  let ex = E.create "hello" in
  printf "%s\n" (E.read ex)
```

```rust
fn client<E: Example>() {
  let ex = E::create();
  println!("{:}", ex.read());
}
```

```haskell
client :: ExampleClass e => IO ()
client = do
  let ex = create "hello"
  print $ read1 ex
```

```fsharp
let E = Example () :> ExampleInterface<string>
let ex = E.create "hello"
E.read ex |> printf "%s\n"
```

</Multicodeblock>

The surface syntax differs among programming languages, but through them all, you can identify a _client_ interacting with an _implementation_ through an _interface_. The extent to which they achieve the ideal, from a semantics perspective, is something we will study in this post. Studying the ideal equips the student with the capacity for applying these techniques across all programming languages rather than relearning what is truly the same each time a new language is presented.

To _really_ understand what an interface does it must be equipped with laws. With sufficient laws, concrete implementations can be swapped without clients observing changes, or dually, clients can be written without implementations existing. "Sufficient laws" gives us both obvious property-based tests and a state known as _representation independence_, but this we will discuss in another post.
