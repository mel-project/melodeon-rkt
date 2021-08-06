# Generic type unification in Melodeon

## Problem description

In Melodeon, there are several places where we need to match _concrete types_ with _parameterized types_. The prototypical example is in invoking generic functions. For example, in the following Melodeon program

```melo
def first_of_two(vec: ['a * 2]) -> 'a = vec[0]
- - -
first_of_two([1, 2])
```

we need to answer "what is the type of `first_of_two([1, 2])`?".

But that requires "matching" `['a * 2]` with `[Bin, Nat]` (the type of `[1, 2]`) and coming to the conclusion that `'a = Nat`. This is a very much nontrivial task --- already we see that simple pattern-matching (which other languages often use) cannot work.

(Note: by saying `'a = Nat`, we mean`'a <: Nat` and `Nat <: 'a`, not literal equality of representation. For example, `'a` is allowed to take on types like `Nat | Bin`, `(Nat | Foobar) & ~Foobar`, etc, because all of them represent the same set of values as `Nat`. All these are essentially _different ways to write down the same type_.)

Another case is in vector indexing. Consider

```melo
def maybe_vec() -> [Nat * 10] | Nat = ...
- - -
if maybe_vec() is Nat then
    0
else
    // maybe_vec has type ([Nat * 10] | Nat) & ~Nat
    maybe_vec()[0] * 2
```

Here, we must unify `([Nat * 10] | Nat) & ~Nat` with a vector type.

## How to proceed?

We do not have a Prolog-like thing to query things like "the smallest supertype of T that looks like a vector of `Nat`s". In fact, such a system would be extremely difficult to implement correctly due to every Melodeon type having an infinite number of representations: `Nat` can be `Nat | Nat | Nat`, `Nat & Nat | Nat & ...`, etc. Intuitions like "get all the supertypes and sort them" are not going to work.

Instead, we lean heavily on two key ideas:

1. **Directly construct types with the set operators rather than calculating them**. For example, to answer "what's the biggest type that is included in both `[Nat * 2]` and `[Any, Bin]`", we don't need to somehow infer `[Nat, Bin]`. The answer's just `[Nat * 2] & [Any, Bin]` --- because it represents the same set of values as `[Nat, Bin]`, you can e.g. pass it to a function that wants `[Nat, Bin]`.
2. **Typechecking failures can be represented by a `Fail[..]` type**. `Fail[fail-reason]` is a special class of type that represents typechecking failure. For example, `Fail[cannot-index-Nat]` might be internally "the type of `2[1]`". As a special case, `Fail[...]` is not included in `Any` --- `Any` can be thought of as "any type but a Fail type".

   This is surprisingly powerful because in a lot of cases, some rewriting rule holds only if all the functions are "infallible". For example, given the indexing function `index : Type, Nat -> Type`, `f(T | U, i) = f(T, i) | f(U, i)`, but only if `f` is infallible (i.e. both `T` and `U` are indexable types). As we will soon see, this lets us defer failure checking --- which is simply making sure the resulting type is a subtype of `Any` --- after we check the whole type, greatly simplifying error handling in cases where whether an apparent error is really an error cannot be determined "locally".

## An example: vector indexing

As an example, how is vector indexing done?

We first define the following, relatively straightforward **infallible** indexing function, which takes in a type `T` and an integer `i`, and returns the type of a vector in `T`'s `i`th element. In pseudo-haskell, it looks like:

```haskell
rawIndex :: Type -> Integer -> Type

rawIndex Nat _ = Fail "nat"
rawIndex Bin _ = Fail "bin"
# Indeterminate is the "true" Any that includes all Fail types
rawIndex Any _ = Indeterminate
# union, intersect, negate
rawIndex (T | U) i = (rawIndex T i) | (rawIndex U i)
rawIndex (T & U) i = (rawIndex T i) & (rawIndex U i)
rawIndex (~ T) = case (f) of (rawIndex T i)
                 | Fail t -> ~ (Fail t)
                 | _ -> Indeterminate
```

**Oh no! This idea cannot work!**
