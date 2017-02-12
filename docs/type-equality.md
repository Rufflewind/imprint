# Type equality

## Definition

It's important to know what *equality* really means here.

Naturally, a type `T` is always equal to itself by reflexivity.  However, such a definition is too weak to be useful.

Consider for a moment that I might be implementing a wrapper type `Wrap<T>`, but the type parameter `T` is *phantom* (a dummy marker type).  What if, as the implementor of `Wrap`, I want to *assert* the fact that `Wrap<Mew>` and `Wrap<Mow>` are truely equivalent?  That is, in any given program that uses `Wrap<Mew>`, a similar program that uses `Wrap<Mow>` instead of `Wrap<Mew>` would yield identical semantics, no matter what program it is!

Hence, a more general definition would be something like: Two types `A` and `B` are equal if and only if, given an arbitrary program `P<A>` (a program template `P` which uses `A`), we can construct another program `P<B>` (the same program template `P`, but uses `B` instead `A`).  The "arbitrariness" of the program `P` is crucial: we aren't saying that it works just for your particular program template `P`, but it must work for every possible program template in existence!

This is in fact the definition of Leibniz equality.  In Haskell:

~~~haskell
data Equal a b = Equal (forall p . p a -> p b)
~~~

In some futuristic Rust, it might look like:

~~~rust
struct Equal<A, B>(for<P> Fn(P<A>) -> P<B>);
~~~

where the idea of "program template" is translated into a type constructor `P`.

## Expressing equality in Rust

Unfortunately we can't express this in Rust for several reasons:

  - We can't abstract over type-level functions (a.k.a. type constructors) such as `P`.
  - We can't store arbitrary functions without using traits.
  - We can't use `for<P>` because higher-ranked types only exist for lifetimes.

The first restriction is a rather fundamental limitation due to the lack of higher-kinded types.  We can sort of work around it using traits: define a trait `Tf` acts as a dispatcher for type-level functions:

~~~rust
trait Tf<F> {
    type Output;
}
~~~

Then, we can define arbitrary type functions like this:

~~~rust
struct VecTf; // a dummy type to label our type function

impl<T> Tf<VecTf> for T {
    type Output = Vec<T>;
}
~~~

Leibniz equality can then be re-expressed as:

~~~rust
struct Equal2<A, B>(for<P> Fn(A::Output) -> B::Output where A: Tf<P>, B: Tf<P>);
~~~

The second restriction is hard to work around.  For now, let's simplify `fn()` instead of `Fn()` -- it won't really matter in a bit.

The third restriction is even worse.  Given that only lifetime polymorphism is supported, we might be tempted to try:

~~~rust
struct Equal3<A, B>(for<'p> Fn(A::Output) -> B::Output where A: Tf<Pl<'p>>, B: Tf<Pl<'p>>);
~~~

where `Pl<'p>` is an alias for `PhantomData<Cell<&'p mut ()>>`.  But then, we run into two problems:

  - We can't define `Tf` anymore, because we can't produce *named* lifetimes out of thin air.
  - The `where` clause is stuck inside the `struct`, but Rust doesn't allow constraints inside the constructor.  Doing so would require some sort of generalized algebraic data types (GADTs).

Okay, fine let's just give up on the idea of storing it in a struct for now.  Instead, let's use a trait:

~~~rust
trait Equal4<A, B> {
    fn apply<P>(self, A::Output) -> B::Output where A: Tf<P>, B: Tf<P>;
}
~~~

We can't turn our trait into a trait object because it uses a generic function ... oh well.  Let's keep trying to use it anyway.

Suppose I want to prove reflexivity:

~~~rust
struct Refl<A>(PhantomData(*mut A));

impl<A> Equal4<A, A> for Refl<A> {
    fn apply<P>(self, x: A::Output) -> A::Output where A: Tf<P> {
        x
    }
}
~~~

That's pretty easy.  Now let's prove symmetry:

~~~rust
impl<A, B> Equal4<A, B> for ?? where Equal4<B, A> {
    fn apply<P>(self, x: A::Output) -> A::Output where A: Tf<Pl<'p>> {

        // start with Equal4<B, B>
        let refl: Refl<B> = Refl(PhantomData);

        // then use a type function to go from
        // Equal4<B, B> to Equal4<A, B>
        // but wait ... these are traits -- we can't define trait functions!
        self.apply::<???>(refl)
    }
}
~~~

So we're stuck: we cannot define trait functions.  The definition of `apply` isn't general enough to allow us to infer `Equal4<A, B>` from `Equal4<B, B>`, given `Equal4<B, A>`.  Had we defined equality using a `struct`, then we'd not have run into this issue, although the flaw still exists because we simply cannot abstract over traits.  The `Tf` trick we used only works for types.

It is tempting to work around this by "demoting" trait implementations into a `struct`:

~~~rust
struct Equal4Impl<A, B, E>(E, PhantomData<*mut (A, B)>) where E: Equal4Impl<A, B>;
~~~

But that doesn't work, because these are like Haskell's DataTypeContexts: they only *impart* constraints and can never be used to *fulfill* constraints.  In Haskell we would use a GADT-style declaration:

~~~rust
struct Equal5Impl<A, B, E>(E, PhantomData<*mut (A, B)> where E: Equal4Impl<A, B>);
~~~

which isn't allowed in Rust, but it *would* solve our dilemma earlier while trying to prove symmetry.

By now, we have a nice wishlist for Rust:

  - Higher-ranked types: 'nuff said.  Won't be easy though.
  - Higher-kinded types: being able to abstract over type functions is nice but there are workarounds.  Being able to abstract over *traits* would be amazingly useful, however.
  - Named dummy lifetimes: it won't save us here, but it could be useful for other things.
  - Allow data to hold constraint evidence (GADT-like): this would actually be fairly simple to support as long as we don't allow higher-ranked types.  Later on during monomorphization we'd just pick based on the concrete types.  There is a possibility that we might not find an implementation if the data was actually forged (e.g. transmutes, diverging functions), in which case we just fail with a compile error.  The main advantage of this is that we can manipulate traits as mere types, rather than having to add a separate system for trait functions.  The downside is that it introduces a possibility for compile errors during the instantiation phase, even if the generic code typechecks.
  - Even better: build a real equality trait into the compiler!  However, the usefulness of such a constraint is rather limited for the same reasons earlier: we need to allow data to hold equality evidence so we can manipulate them easily.

Anyway, the real implementation of type equality is a lot more scary.  We use a concrete data type:

~~~rust
struct Equal<A, B>(...)
~~~

but we do not try to store the Leibniz equality evidence anywhere.  Instead, we just define `apply` as a raw transmute and allow `refl` to be only public, safe constructor for `Equal<A, A>`.  The rest of the laws of equality follow directly from these two without the use of any unsafe trickery.  Now every thing can be done at the value level, yay!  Of course, there is still no way to use `Equal<A, B>` to prove, say, `B: Send` from `A: Send`.  Oh well.

## Uses of type equality

The main use of type equality is for handling type-level values, actually.  Type-level values are identified by an existential lifetime parameter, which need not unify even if the underlying values are actually the same.  Therefore, we provide an equality function that tests if the underlying values are the same and, if so, we conjure a type-level proof of their equality.

Because type equality uses transmutes, that means any two values that are asserted to be equal must be *transmute-safe*.  This is fine, since all we're doing is swapping out a phantom lifetime parameter.  So in terms of memory safety this is all OK.

The problem is whether it's *semantically* valid to equate the lifetime parameter for two equal values.  The answer is: it depends on whether `T` is implemented sensibly.  If `T` has a sensible notion of equality, then this transmutation will help unify values that are indeed equal (for whatever meaning of equality `T` decides to use).  OTOH, if `T` has an absurd notion of equality (nonreflexive, nonsymmetric, and/or nontransitive), the phantom lifetime parameter would have no sensible meaning.

Put in another way, if some code depends on a meaningful equality via the lifetime parameter, then `Eq` or `PartialEq` must provide a meaningful equivalence relation.  If they rely on that for potentially *unsafe* behavior, then it would be wise to restrict `T` to only *trustworthy* types that have a sensible notion of equality.
