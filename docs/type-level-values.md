# Type-level values

One of the limitations of type level programming in the C++ style is that all type-level values must be known at compile time.

If you *don't* know the values at compile time, then life is harder.  The purpose of this library is to provide a basic framework to handle the latter.

One of the most powerful features in Rust is that of *higher-ranked trait bounds*.  By repurposing lifetimes to mean something else, we can use the lifetime as a marker that identifies a piece of data.  (Unfortunately, this also results in some really terrible error messages.)  In some sense, the marker acts as an abstract representation of the variable's underlying value, even though we have no idea what that value is.

## Dependent types

Essentially, we're trying to mimic dependent types in Rust.  In your typical garden-variety dependent type theory, there is a thing called the "dependent function type" or "Pi-type":

    Π[a: A] -> B(a)

If you squint a bit, it looks just like an ordinary function type:

    A -> B

But somehow, the return type `B` is replaced by some weird `B(a)` thing, which is apparently parametrized by `a`.  What is `a`?  It's the argument of the function!

    |a| { ... }

How could a *type* `B(a)` be defined in terms of a *value* `a`?  Well, for whatever reason it actually does end up working out!

But I digress.  What occurs in dependent type theory appears fundamentally impossible to emulate in a Rust, where types only exist at compile time and value only exist at run time.

Fortunately, there is a hack to work around this, called "singletons" (which has nothing to do with OOP whatsoever).  The details can be found in Richard Einsenberg and Stephanie Weirich's [Dependently typed programming with singletons](https://doi.org/10.1145/2364506.2364522) paper.

Anyway, the idea is that, instead of the usual dependent function type here:

    KIND     U        U


    TYPE     A --+-> B(a)
                 |
                 |
    TERM     a --+

We break up the arrow in to two separate pieces:

    KIND     U   +----> A*         U
                 |
                 |
    TYPE     A --+  +-> a* ----> B(a*)
                    |
                    |
    TERM     a -----+

What happened?  Well, we *promoted* the value `a` into a type of its own called `a*`, and simultaneously promoted the type `A` into a new kind called `A*`, which contains only `a*`.  In doing so, we replaced `B(a)` with `B(a*)`, which is no longer a value-to-type function, but a type-to-type function!

The right half of the diagram can be done using the ordinary type-level machinery of Haskell, but the left side requires some hackery.  We need the ability to take an object at the term level and promote it to a type that is *unique* per value and contains *solely* that value.  (That's why it's called a singleton type.)  Dual to that, we also need the ability to demote the value back its term-level value.

Just to be clear on this.  This is the terminology I use:

  - `a`: the term-level / demoted / underlying value
  - `A`: the demoted / underlying type
  - `a*`: the singleton type / promoted type / type-level value
  - `A*`: the promoted *kind*

## Singletons in Rust

Rust doesn't allow you to work with kinds, so we have to scrap that idea from the get go.  Moreover, Rust is quite limited with regard to existential types, so we have to emulate *that* by hacking up the lifetime system.

Long story short, the `Val<'x, T>` takes the role of `a*: A*` in the previous diagram.  The `'x` designates the "promoted type" `a*`, whereas `T` designates the "promoted kind" `A*`.

Promotion is done using the `imprint` function, which takes `x: T` and converts it into a `Val<'x, T>` with an existential `'x`.  This could be used directly or wrapped inside `Exists` to hide the `'x` parameter until later.

Demotion is done using the `Value` trait: it takes `Val<'x, T>` and recovers `T`, or `&Val<'x, T>` and recovers `&T`.
