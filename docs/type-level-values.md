# Type-level values

One of the limitations of type level programming in the C++ style is that all type-level values must be known at compile time.

If you *don't* know the values at compile time, then life is harder.  The purpose of this library is to devise a basic framework to handle the latter.

One of the most powerful features in Rust is that of *higher-ranked trait bounds*.  By repurposing lifetimes to mean something else, we can use the lifetime as a marker that identifies a piece of data.  (Unfortunately, this also results in some really terrible error messages.)  In some sense, the marker acts as an abstract representation of the variable's underlying value, even though we have no idea what that value is.

## Dependent types

Essentially, we're trying to mimic dependent types in Rust.  In your typical garden-variety dependent type theory, there is a thing called the "dependent function type" or "Pi-type":

    Π[a: A] -> B(a)

If you squint a bit, it looks just like an ordinary function type:

    A -> B

But somehow, the return type `B` is replaced by some weird `B(a)` thing, which is apparently parametrized by `a`.  What is `a`?  Why … it's the argument of the function!

    |a| { ... }

How could a *type* `B(a)` be defined in terms of a *value* `a`?  Well, for whatever reason it actually does end up working out!

But I digress.  What occurs in dependent type theory appears fundamentally impossible to emulate in a Rust, where types only exist at compile time and value only exist at run time.

Fortunately, there is a hack to work around this, called "singletons" (which has nothing to do with OOP whatsoever).  The details can be found in Richard Eisenberg and Stephanie Weirich's [Dependently typed programming with singletons](https://doi.org/10.1145/2364506.2364522) paper.

Anyway, the idea is that, instead of the usual dependent function type here:

    KIND       Universe                Universe


    TYPE           A ---------+---------> B(a)
                              |
                              |
    TERM     a₁, a₂, a₃, … ---+

where `B(a)` depends on `a: A` and `a` is some element drawn from `A ≡ {a₁, a₂, a₃, …}`, we break up the arrow in to two separate pieces:

    KIND       Universe       +----------> A*              Universe
                              |
                              |
    TYPE           A ---------+   +-> a₁*, a₂*, a₃*, … ----> B(a*)
                                  |
                                  |
    TERM     a₁, a₂, a₃, … -------+

What happened?  Well, we *promoted* every value `a` into a type of its own called `a*`, and simultaneously promoted the type `A` into a new *kind* called `A*`, which contains all the values `a*`.  In doing so, we replaced `B(a)` with `B(a*)`, which is no longer a value-to-type function, but a type-to-type function!

The right half of the diagram can be done using the ordinary type-level machinery of Haskell, but the left side requires some hackery.  We need the ability to take an object at the term level and promote it to a type that is *unique* per value.

Dual to that, we also need the ability to *demote* the value back its term-level value.  There can only be *one* unique value `a` associated with each type `a*`, and that's why these are called singleton types.

Just to be clear on this.  This is the terminology I use:

  - `a`: the term-level / demoted / underlying value
  - `A`: the demoted / underlying type
  - `a*`: the singleton type / promoted type / type-level value
  - `A*`: the promoted *kind*

## Singletons in Rust

Rust doesn't allow you to work with kinds, so we have to scrap that idea from the get go.  Moreover, Rust is quite limited with regard to existential types, so we have to emulate *that* by hacking the lifetime system.

Long story short, the `Val<'x, T>` takes the role of `a*: A*` in the previous diagram.  The `'x` designates the "promoted type" `a*`, whereas `T` designates the "promoted kind" `A*`.

Promotion is done using the `imprint` function, which takes `x: T` and converts it into a `Val<'x, T>` with an existential `'x`.  This could be used directly or wrapped inside `Exists` to hide the `'x` parameter until later.

Demotion is done using the `Value` trait: it takes `Val<'x, T>` and recovers `T`, or `&Val<'x, T>` and recovers `&T`.

## Equality

One difficulty with this approach is that for every value `a` there can be *multiple* promoted types that are *equivalent* (yields the same demoted value) but not equal.  There are two possibilties here:

  - One is that we have `Val<'x, T>` and `Val<'y, T>` and we can verify at run time that they are equal, but at compile time we cannot tell.  In this case, one could work around this by *conjuring* a [type equality](type-equality.md) proof between the two, which essentially equates the two types at type level.  We can do this because we know the lifetimes are fictitious: their meaning is at our mercy.

     As a result this does not really impact safety (at least by itself) even if the `Eq` implementation of `T` is totally bogus.  Instead, if *you* want to trust the meaning of `'x` then it is your responsibility to ask for trust in `T` somehow (e.g. through a trait, which could be unsafe if you rely on it for potentially unsafe behavior).

  - The other possibility is that we have totally different types with totally different in-memory representations `Foo` and `Bar`.  They both implement `Value` and they compare equal, but `Foo` and `Bar` are clearly not equal at the type-level.

    So that's why I have *another* equality type `Equal` which is evidence for the equality in the demoted value, but not evidence of their type equality.

Because of the latter, `Value` has to be `unsafe` to implement.  Otherwise, there is no way you can ever trust the `Equal` proofs.  For example, if you have `Equal<Good1, Bad>` and `Equal<Bad, Good2>`, both `Good1` and `Good2` have correct implementations of `Value` but `Bad` has a totally broken implementation of `Value`, then you can establish `Equal<Good1, Good2>` even if that isn't true!

From here you can then add more relational propositions such as inequality, arithmetic, etc.  It can get pretty tedious but I think it should be possible, mainly because the proofs are all done at the *value* level rather than type level.

Also in many of these relations you have to implement Leibniz substitution manually: e.g. given `a = b` you can turn `a < x` into `b < x` by substitution.  This works fine for `TyEq` but for the more general `Equal` it must be coded directly since `a` and `b` are not necessarily transmute-compatible.  This adds to the boilerplate.

[Be careful of negations though](sound-logic-in-a-turing-complete-language.md), since negations are often not executed their proofs can be forged without any unsafety.

Maybe some of this can be partly automated using macros?

## Existentials

Sometimes you don't really care to track the imprinted tag around.  For something like `Val<'x, T>`, if you want to erase the tag `'x` you just have to unwrap it.  But what if you want to erase one or more tags while also keeping the *proofs*?

~~~rust
(Less<Val<'x, T>, Val<'y, T>>, Val<'x, T>, Val<'y, T>)
~~~

[Existential types](existential-types.md) are useful for this situation, but they are (1) not easy to use, (2) hilariously broken on today's Rust (causes ICEs).

There is a dirty alternative though: you just implement it as a plain old private-constructor type with phantom parameters and create a public constructors that asks for proofs but never actually stores them.  This is what is used to implement the `ix` module.

## Singletons are ugly

Eisenberg said singletons are an ugly hack and I think that's quite true.  In fact singletons in Haskell will hopefully be superseded by the Dependent Haskell project.

The problem with singletons is that it causes a lot of code duplication: instead of having just a plain old “addition” function I now have one for adding regular values (e.g. `usize`) and one for adding tagged values (e.g. `Val<'x, usize>`).  Moreover, this also has to get reflected on the type level, so I also now have a type `Add<X, Y>` as well.  That's part of why this is quite tedious: every operation is not just duplicated, but triplicated!

It's why I kept the `arith` module quite lean on relation operators: I did not want to add `Greater` when `Lesser` works just fine, as I'd have to write even more boilerplate!  Plus, there's always a risk that you might mistype an axiom somewhere…

## A smarter solution?

The other reason why this is tedious is that we don't have an SMT solver / theorem prover to figure out obvious identities for us.  If `x < y` and `y < z` then *obviously* `x < z`, but Rust does not yet have the smarts to figure that out.  This is a much more open-ended problem though.

Ideally, we want the ability to talk about "every day" arithmetic *easily* without compromising the ability to talk about domain-specific operations and relations.  We kind of have the latter, but the former involves making the Rust compiler smart enough to solve proofs for us.

One possibility is to have like a preprocessor that is run at compile time which leverages a theorem prover to construct the tedious proofs and then splices them into the source code for Rust to verify.
