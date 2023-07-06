---
author: Me
date: Jun 8, 2018
tags: [fp, learning]
---

# Resources for Learning FP

There are many paths to Functional Programming Land, and an embarrassment of riches when it comes to learning materials. I've tried my best to hand-pick out some of the better ones I've come across and categorize them according to a given starting point or "use case". I've also tried to sort each section by how easy or accessible they are to beginners. I'm by no means an expert on any of this stuff---I'm still early on on the learning curve myself---but I've at least attempted most of the tutorials listed and what I lack in expertise I'll try my best to make up for in empathy. So if you find you could use some guidance or help getting through any of the material, I'm happy to help.

## What is Functional Programming?

Before assuming that you want to learn FP, it may help to get an inkling as to what it is:

> FP is just programming with functions. Functions are:
>
> 1. Total: They return an output for every input.
> 2. Deterministic: They return the same output for the same input.
> 3. Pure: Their only effect is computing the output.
>
> The rest is just composition you can learn over time.
>
> <div style="text-align:right;">-- <em>John De Goes</em></div>

While that's true enough, it might not be quite enough to get you started. Here are a few bird's-eye overview talks/presentations that I think are helpful or that I've heard recommended:

1. [Learning Functional Programming with JavaScript](https://www.youtube.com/watch?v=e-5obm1G_FY) by Anjana Vakil. A quick, informal presentation of some functional concepts in JS such as functional purity, array methods, immutability and persistent data structures, from a beginner's perspective.
1. [LambdaCast](https://soundcloud.com/lambda-cast) "The podcast about learning functional programming from the perspective of a working developer." Hosted by David Koontz and a handful of guests with varying degrees of functional programming knowledge---I found it very helpful to have a complete beginner's perspective represented in the discussion. I'd recommend the first episode or two to start, but the series as a whole serves as a good companion to learning these concepts.
1. [Susan Potter on _Functional Geekery_](https://www.functionalgeekery.com/episode-61-susan-potter/) (Jump to about the 5:15 mark to skip the sponsor plugs.) About Susan's transition from "smug OOP" systems programmer in Java/C++ to embracing Erlang and Haskell and the benefits of the abstractions that functional programming has afforded her. Some of it is over my head but maybe useful for those coming from similar background to hers.
1. [Functional Design Patterns](https://fsharpforfunandprofit.com/fppatterns/) by Scott Wlaschin. Covers much of the same material, but if you're anything like me you'll benefit from hearing/reading these things explained more than one way.

## Learning Functional Programming in JavaScript

So you're sold on FP as a worthwhile paradigm and you know a bit of JavaScript, how do you get started with functional JS?

1. [JavaScript30: Array Cardio](https://www.youtube.com/watch?v=HB1ZC7czKRs) with Wes Bos. This whole course is a good JavaScript tutorial series, and Wes is good at being entertaining and engaging. I'd recommend this tutorial in particular as a solid introduction to "the gateway drug(s) to functional programming": the array methods of map, filter, sort, reduce. Plus he has exercises to work through which is key.
2. [FunFunFunction: FP in JS](https://www.youtube.com/watch?v=BMUiFMZr7vk&list=PL0zVEGEvSaeEd9hlmCXrk5yUyqUag-n84) (another YouTube series) with MPJ. Stick with the first four episodes for a more in-depth exploration of JS array methods.
3. [FreeCodeCamp](https://learn.freecodecamp.org/javascript-algorithms-and-data-structures/functional-programming) has a whole section on functional JS if you want to test your knowledge. The instant gratification that comes with completing a passing test is 👌. A good challenge to set for yourself is to try to get through as much of the [algorithms](https://learn.freecodecamp.org/javascript-algorithms-and-data-structures/intermediate-algorithm-scripting) problem set as you can without using a `for`-loop.
4. [_Professor Frisby's Mostly Adequate Guide to Functional Programming_](https://mostly-adequate.gitbooks.io/mostly-adequate-guide/) by Brian Lonsdorf (aka Dr Boolean). I **_strongly_** recommend this one, above and beyond all functional JS guides, once you're comfortable enough with the language. Manipulating arrays is great and all, and sometimes very useful, but if you want to get deeper into implementing some functional patterns this is your ticket.
5. [_Professor Frisby Introduces Composable Functional JavaScript_](https://egghead.io/courses/professor-frisby-introduces-composable-functional-javascript) from same author. This is a stop-motion video series featuring woodland creatures in a classroom setting, all while managing to be very educational. It does start a bit in the deep end, but coding along and re-watching a few times are rewarded.
6. [Fantas, Eel, and Specification](http://www.tomharding.me/fantasy-land/) by Tom Harding. A series of blog posts that have proved quite popular though I haven't (_yet_) worked my way through all of them. In the author's words, it's a series "in vanilla JS, going through everything you'd need to know to be a Haskell programmer."

## Learning a Functional Programming Language

If you really want to go all-in 🏊🏼‍♀️ and make the deep dive into a purely-functional, statically-typed language, then I welcome you to come along with me!

1. [_Haskell Programming from First Principles_](http://haskellbook.com) (or _Haskell Book_ for short) by Chris Allen and Julie Moronuki. This is a _very_ long book (I'm currently only 2/3 of the way through after over two months), but it starts from absolute beginner-level and follows a very principled and well-researched approach to learning the language.
2. [Brent Yorgey's CIS 194 course](http://www.seas.upenn.edu/%7Ecis194/spring13/lectures.html) Recommended as the best _free_ resource for learning Haskell (if you don't want to buy _Haskell Book_).
3. [_PureScript by Example_](https://leanpub.com/purescript/read) by Phil Freeman, the author of the PureScript language. Very much Haskell-inspired, so I've heard it recommended to get familiar with the concepts and syntax of Haskell first, since there are more resources for learning the latter.

## Learning the Lambda Calculus

All functional programming languages have at their root the lambda calculus invented (or [_discovered?_](https://www.youtube.com/watch?v=IOiZatlZtGU&t=1345s)) by Alonzo Church in the 1930s. This is a subject worthy of study in its own right, but starting to think in terms of the abstractions involved in combinatory logic can be a very powerful exercise. I think the best introduction I've found was in the first chapter of _Haskell Book_, but since that is not freely available, I've tried to find some alternative resources. A lot of these tutorials cover the same material in different media and with different approaches; I've found that it usually takes a repeated exposure to the concepts presented in a variety of ways before I finally start to develop an intuition.

### Videos

1. [Lambda Calculus (Computerphile video)](https://www.youtube.com/watch?v=eis11j_iGMs&t=23s) with Prof Graham Hutton. A perhaps too-quick run-through of the fundamentals, but he offers a good beginner challenge: implement the logical `AND` operator (`&&` in JavaScript) and the logical `OR` operator (`||`) in terms of the lambda calculus.
2. [Fundamentals of Lambda Calculus](https://youtu.be/3VQ382QG-y4) by Gabriel Lebec. A couple hour-long presentations on the subject, with LC syntax presented alongside JavaScript.

### Reading (online)

1. [Lambda Calculus with JavaScript (Medium article)](https://medium.com/@ahlechandre/lambda-calculus-with-javascript-897f7e81f259) by Alexandre Thebaldi
2. [A Tutorial Introduction to the Lambda Calculus](https://www.inf.fu-berlin.de/lehre/WS03/alpi/lambda.pdf) by Raúl Rojas. Presented as "a short and painless introduction to the λ calculus." I personally found it a little difficult to follow as my first introduction (your mileage may vary), but a good supplement to other more accessible readings.
3. [Introduction to Lambda Calculus](http://www.nyu.edu/projects/barker/Lambda/barendregt.94.pdf) by Henk Barendregt and Erik Barendsen. I haven't tried to get through this.

### Reading (print)

1. [_To Mock a Mockingbird_](http://www.wikiwand.com/en/To_Mock_a_Mockingbird) by Raymond Smullyan. Combinatory logic puzzles. I haven't read this but I've heard it recommended enough times that I thought I'd add it here (also available from at least [one Cape library](http://www.clamsnet.org/search~S1?/Xsmullyan&searchscope=1&SORT=D/Xsmullyan&searchscope=1&SORT=D&SUBKEY=smullyan/1%2C4%2C4%2CB/frameset&FF=Xsmullyan&searchscope=1&SORT=D&2%2C2%2C)).
