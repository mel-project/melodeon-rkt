# Union disambiguation in Melodeon

## Problem statement

As a Typed-Racket-like language with occurrence types, Melodeon at a bare minimum needs union types like `Nat | Bytes[5]`. The question is _what other tools do we need_? In particular, consider the following case:

```
// x here has type Nat | Bytes[5]
if x is Nat then
    x + 1
else
    x[0]
```

We'd like the above code to typecheck properly. A naive approach where `x is Nat` just guarantees that its "downstream" has `x : Nat` will not work for the `else` case.

In other words, we need a way to infer that given `x : Nat | Bytes[5]` and that `x : Nat` is false, `x : Bytes[5]`. There are several ways we can do this:

1. **Full set operators in types**: We can just add intersection and negation types. Then, we can infer that `x : (Nat | Bytes[5]) & ~Nat`. By using a SAT-solving subtype checker, this ugly type is a subtype of `Nat`, and everyone is happy.

- Pros: highly elegant conceptually. Types are just sets/boolean expressions. Other parts of the compiler (like occurrence typing) can just throw work to the typechecker, which throws work to a SAT solver.
- Cons: **inference** becomes extremely tricky. How do you index into the third element of `([Nat * 3] | [Nat * 2]) & ~[Any * 2]`? Expressing this as a boolean formula isn't very helpful, because it will contain quantifiers.
  The general solution involves representing each type as a disjunctive-normal-form **bag of facts** --- an OR between different alternatives, each alternative being an AND of statements ascribing _primitive_ types to elements of a value. For example,

  ```
  [Nat * 3] | [[Any * 2] * 2]
  ```

  translates into

  ```
  /    : Vector AND
  /0   : Nat    AND
  /1   : Nat    AND
  /2   : Nat    AND
  /len : 3

  OR

  /      : Vector AND
  /0     : Vector AND
  /0/len : 2      AND
  /1     : Vector AND
  /1/len : 2      AND
  /len   : 2
  ```

2. **Ad-hoc union deconstruction**. In the above case, we remove from the union all subtypes of `Nat`. This lets us not add anything more to the typechecker.

- Pros: typechecking is no longer NP-complete and can be very fast. People from nominal-typed languages will not feel as big of a culture shock, as the "surface" representation of the type, not abstract sets of values, is what occurrence typing works upon.
- Cons: without inelegant special-cases, the following will not typecheck:

  ```
  // x : [Nat | Point, Nat]
  if x is [Nat, Nat] then
      x[0] + 1
  else
      x[0].x * 2
  ```

  Of course, we can force people to (safely --- the subtype-checker can prove equivalence) annotate as `[Nat, Nat] | [Point, Nat]` first, but that can get annoying.

3. **Intersection types, but not negation types**. This is largely similar to 1., except we are guaranteed that in the bag-of-facts representation, we will never have cases like
   ```
   / : NOT Nat
   ```
   which are much harder to work with. We can still do "set subtraction" through "facts subtraction" --- given bag A and bag B, we remove everything from bag A that is also always true in bag B.

## Current state

Right now we have a horribly half-baked attempt at strategy 1, and perhaps we should move to strategy 3 and a disciplined bag-of-facts representation:

```
(struct Type-Bag (bags : (Setof (Immutable-HashTable Index Primitive-Type))))
```
