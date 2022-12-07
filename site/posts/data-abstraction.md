---
title: "Stateful for loops in Rust & Haskell"
author: Me
date: Jan 1, 2019
tags: [slick, site, data abstraction, haskell, typescript]
description: My first blog post using slick
---

# Stateful `for` loops in Rust & Haskell

**Advent of Code spoilers herein**

Day five of this year's Advent of Code this year provides a nice occasion for doing some stateful computation. We were given:

- an initial state of some stacks of crates
- a list of instructions for moving crates from one stack to another

In Haskell I ended up using the `State` ~~monad~~ applicative functor, and I am compelled to write down why, because it was not that long ago that `State` was completely baffling to me.

```haskell
newtype State s a = State { runState :: s -> (a, s) }
```

How does this really model "state"?—how can we use this the same way that we use a global mutable reference in imperative-style programming.
Let's get some imports out of the way first.

```{=html}
<multicodeblock-tabs>
  <multicodeblock-tab role="heading" slot="tab">Haskell</multicodeblock-tab>
  <multicodeblock-panel role="region" slot="panel">
```

```haskell
import Control.Monad.State (modify)
import Data.Foldable (for_)
import
```

```{=html}
  </multicodeblock-panel>
  <multicodeblock-tab role="heading" slot="tab">Rust</multicodeblock-tab>
  <multicodeblock-panel role="region" slot="panel">
```

```rust

```

```{=html}
  </multicodeblock-panel>
  <multicodeblock-tab role="heading" slot="tab">OCaml</multicodeblock-tab>
  <multicodeblock-panel role="region" slot="panel">
```

```ocaml
(* interface, abstract type: A module type *)
module type Example_intf = sig
  type t

  val create : string -> t
  val read : t -> string
end
```

```{=html}
  </multicodeblock-panel>
  <multicodeblock-tab role="heading" slot="tab">Rust</multicodeblock-tab>
  <multicodeblock-panel role="region" slot="panel">
```

```rust
// interface, abstract type: A trait
trait ExampleTrait {
  fn create(s: &'static str) -> Self
  fn read(&self) -> &'static str
}
```

```{=html}
  </multicodeblock-panel>
  <multicodeblock-tab role="heading" slot="tab">Haskell</multicodeblock-tab>
  <multicodeblock-panel role="region" slot="panel">
```

```haskell
-- interface, abstract type: A typeclass
class ExampleClass a where
  create :: String -> a
  read1 :: a -> String
```

```{=html}
  </multicodeblock-panel>
  <multicodeblock-tab role="heading" slot="tab">F#</multicodeblock-tab>
  <multicodeblock-panel role="region" slot="panel">
```

```fsharp
type ExampleInterface<'a> =
    abstract member create : string -> 'a
    abstract member read   : 'a -> string
```

```{=html}
  </multicodeblock-panel>
</multicodeblock-tabs>
```

### Implementation

```{=html}
<multicodeblock-tabs>
  <multicodeblock-tab role="heading" slot="tab">Swift</multicodeblock-tab>
  <multicodeblock-panel role="region" slot="panel">
```

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

```{=html}
  </multicodeblock-panel>
  <multicodeblock-tab role="heading" slot="tab">TypeScript</multicodeblock-tab>
  <multicodeblock-panel role="region" slot="panel">
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

```{=html}
  </multicodeblock-panel>
  <multicodeblock-tab role="heading" slot="tab">OCaml</multicodeblock-tab>
  <multicodeblock-panel role="region" slot="panel">
```

```ocaml
(* implementation: A module *)
module Example : Example_intf = struct
  type t = string

  let create s = s
  let read s = s
end
```

```{=html}
  </multicodeblock-panel>
  <multicodeblock-tab role="heading" slot="tab">Rust</multicodeblock-tab>
  <multicodeblock-panel role="region" slot="panel">
```

```rust
// implementation: A struct
pub struct Example {
  s: &'static str
}
impl ExampleTrait for Example {
  fn create(s: &'static str) -> Example {
    return Example{ s }
  }

  fn read(&self) -> &'static str {
    return self.s
  }
}
```

```{=html}
  </multicodeblock-panel>
  <multicodeblock-tab role="heading" slot="tab">Haskell</multicodeblock-tab>
  <multicodeblock-panel role="region" slot="panel">
```

```haskell
-- implementation: A typeclass instance
instance ExampleClass String where
  create = id
  read1 = id
```

```{=html}
  </multicodeblock-panel>
  <multicodeblock-tab role="heading" slot="tab">F#</multicodeblock-tab>
  <multicodeblock-panel role="region" slot="panel">
```

```fsharp
type Example () =
    interface ExampleInterface<string> with
        member _.create s = s
        member _.read s = s
```

```{=html}
  </multicodeblock-panel>
</multicodeblock-tabs>
```

### Client

```{=html}
<multicodeblock-tabs>
  <multicodeblock-tab role="heading" slot="tab">Swift</multicodeblock-tab>
  <multicodeblock-panel role="region" slot="panel">
```

```swift
func client<E: ExampleInterface>() {
  let ex = E.create("hello");
  println(ex.read());
}
```

```{=html}
  </multicodeblock-panel>
  <multicodeblock-tab role="heading" slot="tab">TypeScript</multicodeblock-tab>
  <multicodeblock-panel role="region" slot="panel">
```

```typescript
// client
// it's a bit hard to write down a generic function for this in TypeScript
// because there are two separate interfaces
// ...
const ex = createExample(Example, "hello");
console.log(ex.read());
```

```{=html}
  </multicodeblock-panel>
  <multicodeblock-tab role="heading" slot="tab">OCaml</multicodeblock-tab>
  <multicodeblock-panel role="region" slot="panel">
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

```{=html}
  </multicodeblock-panel>
  <multicodeblock-tab role="heading" slot="tab">Rust</multicodeblock-tab>
  <multicodeblock-panel role="region" slot="panel">
```

```rust
fn client<E: Example>() {
  let ex = E::create();
  println!("{:}", ex.read());
}
```

```{=html}
  </multicodeblock-panel>
  <multicodeblock-tab role="heading" slot="tab">Haskell</multicodeblock-tab>
  <multicodeblock-panel role="region" slot="panel">
```

```haskell
client :: ExampleClass e => IO ()
client = do
  let ex = create "hello"
  print $ read1 ex
```

```{=html}
  </multicodeblock-panel>
  <multicodeblock-tab role="heading" slot="tab">F#</multicodeblock-tab>
  <multicodeblock-panel role="region" slot="panel">
```

```fsharp
let E = Example () :> ExampleInterface<string>
let ex = E.create "hello"
E.read ex |> printf "%s\n"
```

```{=html}
  </multicodeblock-panel>
</multicodeblock-tabs>
```

The surface syntax differs among programming languages, but through them all, you can identify a _client_ interacting with an _implementation_ through an _interface_. The extent to which they achieve the ideal, from a semantics perspective, is something we will study in this post. Studying the ideal equips the student with the capacity for applying these techniques across all programming languages rather than relearning what is truly the same each time a new language is presented.

To _really_ understand what an interface does it must be equipped with laws. With sufficient laws, concrete implementations can be swapped without clients observing changes, or dually, clients can be written without implementations existing. "Sufficient laws" gives us both obvious property-based tests and a state known as _representation independence_, but this we will discuss in another post.

We can concisely communicate these laws through the use of interfaces that express algebraic structures. With enough practice, our whole industry can instantly be aware of some structures' associated laws just through name recognition.

We can imagine a tower we can attempt to climb whenever working on some piece of new code:

On the bottom, there is that which is is formed with the least effort, buckets of plain code. We can add more order through interfaces. Further refine it with laws. And finally, lighten the burden of needing to constantly revisit laws through the identification of algebraic structures.

Sometimes we'll be unable to identify an algebraic structure, perhaps we don't want to put the time into discovering the laws, and we're just prototyping or writing glue so we don't want to come up with interfaces. But when necessary, the tower I've shown here gives us a strategy for simplifying pieces of code.

In this post, we'll focus only on the third layer, interfaces. Note that we've already talked a bit about the top layer in earlier posts starring [algebraic](/posts/semigroups-and-monoids) [structures](/posts/reducers-are-monoids). The second layer will be discussed in a follow-up post.

## Idealized Data Abstraction

As stated earlier, understanding the concepts in your chosen language is useful now, but understanding them from a formal perspective will persist through your career. This post will show how these idealized forms manifest in mainstream languages as a tool for better internalizing these concepts.

To motivate the right way to think about abstract data types (interfaces), I want to contrast it to working with parametric polymorphism which you may know this as programming with "generic"s or "template"s.

Consider a function that takes in a list of arbitrary elements, $A$, and returns the length.

When implementing the body of such parametrically polymorphic functions we're constrained to not rely on the type of $A$. In return, callers of our function have the liberty to choose any $A$ &mdash; in our case they can pass a list of `int`s a list of `string`s or a list of `frog`s. This is also known as _universal quantification_ of our type variable, $A$.

When defining such generic functions we're defining a family of functions: One for each choice of concrete type. This family is, in a sense, an infinite product[^2] of all such functions.

Consider an abstract data type representing an integer stack that is never empty. What we're describing is "some stacklike thing" that can push and pop integers.

When implementing our functions we have the liberty to rely on the concrete type. Essentially, the self is a parameter for some of these functions. The self passing is implicit in many languages, but, interestingly, very explicit in Rust. In contrast, users of our interface, callers of create, push, and pop, are constrained to not be able to rely on the concrete type of the stack.

When defining such abstract data types in a sense we're defining a family of constructors for data types: One for each choice of concrete implementation as we can forget the details that make these implementations unique. This family is, in a sense, an infinite sum; we have one variant for each concrete implementation.

In this way, parametric polymorphism is dual to data abstraction.

Through the Curry-Howard isomorphism[^4] a generic $A$ in our types correspond to $\forall A$ in logic. In other words, a universally quantified type variable in type theory is isomorphic to a universally quantified propositional variable in logic. The dual of $\forall$ is $\exists$ or "there exists." Now we can go backward through Curry-Howard and land on the irrefutable conclusion that _abstract types are existential types_. There exists some concrete stack, where the implementor knows the underlying concrete representation, but as the client, we don't know the details. We _existentially quantify_ over the type of the concrete representation.

### Interface

Our idealized form of data abstraction will refer to abstract data types as $\exists t.\tau$ where $\tau$ stands in for some time that depends on $t$. Concretely for stacks: $\exists t. \langle create : int \rightarrow t, push: t \rightarrow int \rightarrow t, pop: t \rightarrow (int \times t) \rangle$. In English, you may say, there exists some stack, `t`, with a create function from `int` to `t`, a push function from `t` and `int` to `t`, and a pop function from `t` to `int` and `t`.

### Implementation

We can pack a chosen representation type, $\rho$, along with an implementation $e$ replacing $\rho$ for $t$ in our existential box to create an abstract data type (or introducing a new variant to our infinite sum) $\bigcup \rho; e; \exists t.\tau$[^5]. Concretely for the stack example we can choose $\rho$ to be the default int and a list storing the values pushed so far: $\bigcup (Int * List[Int]) ; \langle create = \dots, push = \dots, pop = \dots \rangle ; \exists t. \langle create : unit \rightarrow t, push: t \rightarrow int \rightarrow t, pop: t \rightarrow (int \times t) \rangle$

### Client

A client is an expression that opens a packed value for use under an environment where the choice of the existential $t$ is opaque. The client must be able to run _for all_ specific implementations. Due to these requirements, it's best to think of a client as a function[^6] of type $\forall t. \tau \rightarrow \tau_2$. Note, we add a further restriction that $t$ cannot show up in the return type $\tau_2$. We'll show below how this restriction increases the power of our abstract data type.
Concretely for the stack example: a function that pops two ints off of our stack and returns their sum would have type $\forall t. \langle create : unit \rightarrow t, push: t \rightarrow int \rightarrow t, pop: t \rightarrow (int \times t) \rangle \rightarrow int$

Recall that these idealized forms manifest themselves with a subset of their power in our programming languages as shown below:

## Properties of Abstract Data Types

In this section, we'll enumerate a few interesting properties of abstract data
types first in their idealized forms and then in our mainstream languages. If you only want to see languages that can properly express all of these properties, skip to the OCaml or ReasonML versions of these code samples.

### Implementations as values

In an ideal world, a packed implementation is a value. It is first-class. We can create it in an expression anonymously, we can accept it as an argument to a function, and we can return it from a function as well. $\bigcup \rho; e; \exists t.\tau$ can appear anywhere any other expression can appear.

Note: The seeming lack[^7] of power of Haskell is just due to this section occurring before I explain Rank2 types.

This property provides many interesting capabilities that we won't enumerate in full. Here's just one: Coupled with the rule restricting the existentially quantified $t$ from appearing in the result of client functions, we can use the first-classed-ness to allow for the unification of disparate concrete types in branches as long as they pack to the same interface. Our machines use dynamic dispatch to make this work at runtime.

Now, the rank2 `UI` type:

```haskell
newtype UI a v = UI (forall component. (Reducer a component -> v))
```

This type represents a UI at this moment. A `UI` is fed a function for firing actions `a` to evolve the component and in response returns some representation of a view.

```haskell
newtype Component w m v = Component (w (UI (m ()) v))
```

Think of a component as a space of all possible future instantaneous UIs with a pointer at the current one. You can avoid thinking about the `w` in too much detail; just know that the `w` defines the space of possible futures. `m` defines a way to transition between the states.

![diagram showing the space of states normal, green, blank and a pointer to normal](/static/posts/data-abstraction/basic1.pdf.png)

Notice that the `UI` doesn't depend directly on `Component` but does so indirectly through a rank2 type. When these types are driven by the runtime, the quantified `component` is replaced by the `Component` that the `UI` was originally a part of. It is a corecursive process that goes on forever &mdash; this is what we want as we don't want our UI to ever stop responding to actions.

The rank2 type here grants us the ability to not need to talk about the `w` and `m` type parameters backing the `Component` whenever we only want to concern ourselves with the `UI`. We've simplified the interface for consumers of the library.

I'll explain these data types further as well as the comonadic UI framework as a whole in later posts.

## Conclusion

Data abstraction helps us work on large codebases with ourselves and others by giving us tools to share and reuse code more easily. The Tower of Taming Complexity argues we can clarify code with interfaces, clarify interfaces with laws, and clarify lawful interfaces with algebraic structures. The programming languages we use every day have a way to express _interfaces_, _implementations_, and _clients_, but rather than thinking about the theory of data abstraction through our favorite language, we use an idealized one. Idealized data abstraction, thinking about abstract data types as the dual to parametric polymorphism, as existentials, shows us not only what we can achieve in our existing languages today but what we hope to achieve in future ones. Finally, we saw that existential types can be expressed with rank2 universal types and dug a slightly deeper into the comonadic UI framework.

Next time, we'll cover the part of the tower on lawful interfaces. We'll dig into representation independence and discuss mechanically discovering laws associated with interfaces. Plus how those laws guide us towards nice property-based tests for our code. Thus, granting us the courage within us to refactor without fear.

Thank you [Chris Eidhof](https://twitter.com/chriseidhof) and [Daira Hopwood](https://twitter.com/feministPLT) for pointing out some mistakes in early versions of this post! Thank you [Janne Siera](https://twitter.com/SieraSolutions) for adding F# examples to the post!

### Sources

I heavily relied on Mitchell and Plotkin's "Abstract Types Have Existential Type" and Chapter 17 of Practical Foundations of Programming Languages (PFPL) by Bob Harper, and, of course, Wikipedia when writing this post. "Abstract Types Have Existential Type" more thoroughly talks through the different forms of composition and power abstract types have and PFPL introduces the existential, pack, and open syntactic forms, shows typing rules and provides one take on the representability with rank-2 types. I intended to repackage these pieces of information in a simplified manner and reflecting on how this theory manifests within mainstream programming languages. If you want to learn more, I recommend reviewing these sources.

[^1]: I squeezed a lot into this metaphor. Think of people on a team as threads. Even in a single-threaded system, concurrency is useful, when working with slow IO for example, and with the right tools, it is manageable. The prudent use of interfaces makes it easier to work with other people or switch between code even on a solo project.
[^2]: Pun intended.
[^3]: Otherwise known as a tuple. The number of inhabitants of a product of $N$ types is the product of the number of inhabitants of each of the $N$ types themselves. $Bool \times Bool$ has $4$ inhabitants because $Bool$ has two, `True` and `False`, and $2 \times 2 = 4$.
[^4]: The Curry-Howard isomorphism allows us to teleport types in our programming languages back and forth with theorems in logic; values with proofs. This is a very fascinating property of our universe and I encourage you to explore it! But that is not what this post is about. To learn more see the [wikipedia page](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence) on the subject.
[^5]: For typesetting purposes, I chose a union symbol since packing an implementation is like a variant constructor for the infinite sum that the interface represents. In other literature, you may see this represented with the operator "pack".
[^6]: In order to simplify the presentation (and further explanations below), I chose to think about clients as they're expressed in System-F. Typically, in other literature, you will see this represented as "open" or "abstype".
[^7]: I'm sure there's some obscure extension that can support first-class packed implementations, but I needed a transition to the material later, so please let me tease it here.
[^8]: See Practical Foundations of Programming Languages (PFPL) by Bob Harper on page 155
[^9]: I am pretty sure I learned this trick by reading through one of [Phil Freeman](https://twitter.com/paf31)'s original implementations of Comonadic UI, but I am unable to find the source.
