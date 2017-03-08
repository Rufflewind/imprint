# Higher-ranked types

It would be great to not have to use lifetimes *solely* for the purpose of using higher-ranked trait bounds:

  - Lifetimes are not as rich as types: you can't name arbitrary lifetimes (other than `'static`) or compose new lifetimes from existing ones.
  - Lifetimes tend to linger as a trait bound even after erasing them (e.g. trait objects).
  - Abusing lifetimes causes some really awful compile errors.

What if we [amended the type system to support higher-ranked trait bounds with *type* parameters](https://github.com/rust-lang/rfcs/issues/1481)?

Better yet, what if they are allowed on ordinary types, not just trait bounds?

## Universally quantified types in trait bounds

Let’s not get too far ahead of ourselves and start with the “simplest” extension: allow `for<T>` inside a trait bound:

~~~rust
struct Foo<T: ?Sized>(PhantomData<*mut T>);

fn with_foo<F, R>(f: F) -> R
    where F: for<T: ?Sized> FnOnce(Foo<T>) -> R;
~~~

But as it turns out, we can *already* encode this in Rust, albeit more verbosely:

~~~rust
pub trait FooCallback {
    type Output;
    fn foo_callback<T: ?Sized>(self, foo: Foo<T>) -> Self::Output;
}

pub fn with_foo<F: FooCallback>(f: F) -> F::Output;
~~~

The users won’t like this clunky interface, because they have to define an `impl` at every call rather than just passing a simple (polymorphic) closure.  Still, for all intents and purposes this remains within the spirit of a universally quantified type, just not very ergonomic.

How are we going to implement `with_foo`?  The naive solution would be something like:

~~~rust
pub fn with_foo<F: FooCallback>(f: F) -> F::Output {
    f.foo_callback(Foo(PhantomData))
}
~~~

But if we tried to compile this it would fail with a type ambiguity :(  Yet if I wrote the same thing in Haskell, it would accept it just fine!

Since Rust implements generics through monomorphization, it *has* to precisely which `T` you want.  In this particular case, we don’t really care what `T` is, so Rust can just pick something at random, right?

Nope, [type parameters are *not* parametric in Rust.](https://github.com/rust-lang/rust/issues/27749)  This means the choice of `T` can *fundamentally* change the behavior of the program.

Oh well, that sucks.  Let’s just specialize it to some arbitrary type of our choice:

~~~rust
pub fn with_foo<F: FooCallback>(f: F) -> F::Output {
    f.foo_callback(Foo::<()>(PhantomData))
}
~~~

No big deal right?  The callback `f` can’t *possibly* figure out what `T` we picked!

Wrong again… The lack of parametricity is precisely what allows *information leakage*: it means the callback `f` *can* acquire information about the identity of `T`.  Thanks to the new specialization feature, we can figure out whether `T` was instantiated with `()`:

~~~rust
// requires: #![feature(specialization)]

trait TryIntoFooUnit {
    fn try_into_foo_unit(self) -> Option<Foo<()>>;
}

impl<T: ?Sized> TryIntoFooUnit for Foo<T> {
    default fn try_into_foo_unit(self) -> Option<Foo<()>> { None }
}

impl TryIntoFooUnit for Foo<()> {
    fn try_into_foo_unit(self) -> Option<Foo<()>> { Some(self) }
}

struct Reveal;

impl FooCallback for Reveal {
    type Output = Foo<()>;
    fn foo_callback<T: ?Sized>(self, foo: Foo<T>) -> Self::Output {
        foo.try_into_foo_unit().unwrap()
    }
}

fn main() {
    let stolen_foo: Foo<()> = with_foo(reveal);
}
~~~

You might argue that this can be easily avoided by choosing a private type instead of `()`:

~~~rust
pub fn with_foo<F: FooCallback>(f: F) -> F::Output {
    struct SecretType;
    f.foo_callback(Foo::<SecretType>(PhantomData))
}
~~~

Alas, this does not make it any less leaky.  Consider this example, where I have used the nonparametricity of `T` to *unify* `Foo<T>` and `Foo<U>` from distinct instantiations!

~~~rust
// requires: #![feature(specialization)]

trait TryIntoFooOther<T: ?Sized> {
    fn try_into_foo_other(self) -> Option<Foo<T>>;
}

impl<T: ?Sized, U: ?Sized> TryIntoFooOther<U> for Foo<T> {
    default fn try_into_foo_other(self) -> Option<Foo<U>> { None }
}

impl<T: ?Sized> TryIntoFooOther<T> for Foo<T> {
    fn try_into_foo_other(self) -> Option<Foo<T>> { Some(self) }
}

struct ShapeshiftInner<T: ?Sized>(Foo<T>);

impl<T: ?Sized> FooCallback for ShapeshiftInner<T> {
    type Output = ();
    fn foo_callback<U: ?Sized>(self, foo: Foo<U>) -> Self::Output {
        let mut v = Vec::new();
        v.push(self.0);
        v.push(TryIntoFooOther::<T>::try_into_foo_other(foo).unwrap());
        // now v contains both of the foo's!
    }
}

struct ShapeshiftOuter;

impl FooCallback for ShapeshiftOuter {
    type Output = ();
    fn foo_callback<T: ?Sized>(self, foo: Foo<T>) -> Self::Output {
        with_foo(ShapeshiftInner(foo));
    }
}

fn main() {
    // This is a very obfuscated way of writing:
    //
    //     with_foo(|foo1| with_foo(|foo2| {
    //         let mut v = Vec::new();
    //         v.push(foo1);
    //         v.push(foo2.try_into_foo_other().unwrap());
    //     }));
    //
    with_foo(ShapeshiftOuter);
}
~~~

Why does any of this this matter?

Because it literally destroys one of the most exciting applications of the universal quantifier: to generate fake types on-the-fly by branding them with a unique (“skolem”) type parameter shared by no other.

In some sense, this is largely because we’ve been cheating all along and Rust’s type system is very honest and transparent: we have always used a *real* concrete type (be it `()` or `SecretType`) as the argument of our `with_foo`, so the fact that Rust ultimately allows us to unify different instantiations of `Foo<SecretType>` with each other should not surprise us.

What this demonstrates is that:

  - We can already emulate universal quantification over type variables in trait bounds safely using Rust’s trait system.
  - Merely having universally-quantified types in trait bounds isn’t particularly gamebreaking.  It does *not* allow us to safely generate fake types because Rust type parameters are nonparametric.

## Fictitious types and parametricity

To generate legitimately fake types, we need a new mechanism to enforce parametricity.

One idea is to introduce an OIBIT called `Concrete` (possible alternate names: `Known`, `Identifiable`,  [`Reflect`](https://github.com/rust-lang/rust/issues/27749)): the idea is that if you ever want a truly parametric type parameter, you relax the bound with `?Concrete + ?Sized`.

A `Concrete` type is any type whose identity is fully known at the monomorphization stage.  Therefore, there is a point in the compiler’s pipeline at which you can query `Concrete` types about every property imaginable:  “Is it `Sized`?”  “Is it `Debug`?”  “Is it equal to another `Concrete` type?”  etc.  In contrast, for a `?Concrete` type, asking such questions may or may not yield definite answers.

All types in Rust today that we know and love are `Concrete`.  However, in some situations, the compiler may generate a new unknown type on the fly that does not satisfy the `Concrete` bound, or any bound for that matter.  Call these transient types **fictitious types** (“skolem types”).

There is precisely one situation in which the compiler can create fictitious types: if, after type inference, there remain uninferrable types that are ambiguous, and those uninferrable types are totally unconstrained (no `Concrete` bounds, no `Sized` bounds, nothing whatsoever), then each of those uninferrable types is inferred as a fresh, fictitious type.  “Fresh” means that the type does not unify with anything but itself.

Fictitious types only play a role in type-checking.  During monomorphization, they are simply replaced with a dummy type such as `()`.

Consider the example earlier, now updated with the appropriate bounds:

~~~rust
struct Foo<T: ?Concrete + ?Sized>(PhantomData<*mut T>);

pub trait FooCallback {
    type Output;
    fn foo_callback<T: ?Concrete + ?Sized>(self, foo: Foo<T>) -> Self::Output;
}

pub fn with_foo<F: FooCallback>(f: F) -> F::Output {
    f.foo_callback(Foo(PhantomData))
}
~~~

In this case, no ambiguity error would be generated.  Instead, `T` would be automatically inferred as a fresh fictitious type.

How do fictitious types interact with specialization?  Suppose I try the specialization trick earlier:

~~~rust
trait TryIntoFooOther<T: ?Concrete + ?Sized> {
    fn try_into_foo_other(self) -> Option<Foo<T>>;
}

impl<T: ?Concrete + ?Sized, U: ?Concrete + ?Sized> TryIntoFooOther<U> for Foo<T> {
    default fn try_into_foo_other(self) -> Option<Foo<U>> { None }
}

impl<T: ?Concrete + ?Sized> TryIntoFooOther<T> for Foo<T> {
    fn try_into_foo_other(self) -> Option<Foo<T>> { Some(self) }
}
~~~

After desugaring, the two cases being considered are:

    TryIntoFooOther<U> for Foo<T>
    TryIntoFooOther<U> for Foo<T> where U == T

If `U` and `T` are `?Concrete`, then there is a risk that they may be instantiated with fictitious types at some point.  Since information about ficitious types is lost by the time monomorphization occurs, it becomes impossible to determine whether `U == T` is true or not.  To mitigate this, we add a rule to forbid specializations that contain *non-concrete* bounds.  A bound is said to be *concrete* if there exists a definite yes/no answer at monomorphization.  More specifically:

  - (A) A bound is concrete if every type parameter is `Concrete`.
  - (B) A bound is concrete if there exists a matching implementation.
  - (C) A bound is concrete if there can never be a matching implementation.

Examples:

  - `T == T` is concrete (Rule B, if we treat `==` as a built-in trait).
  - If `T: Concrete` and `U: Concrete`, then `T == U` is concrete (Rule A).
  - If we have a blanket `impl<T: ?Concrete + ?Sized> Qux for T`, then `T: Qux` is concrete (Rule B), and `T: !Qux` is concrete (Rule C).

It is permissble to use non-concrete bounds in other situations, because the burden is on the user to prove the validity of the bound.  However, their presence in specializations is expressly disallowed because that burden would be shifted to the compiler.

A fully expanded type is `Concrete` if and only if all its type parameters are `Concrete`.  (“Fully expanded” here means after substituting all the type aliases.)

    T: Concrete … ⇒ F<T …>: Concrete    where F: Generative

There is a subtle interaction between `Sized` and `Concrete`: although not all `Sized` types are `Concrete`, every type that is `Sized` and `!Concrete` has a size that is known independent of its fictitiously instantiated parameters.

There are several primitive types that can hold `!Concrete` types.  Raw pointers:

    T: ?Concrete + ?Sized ⇒ *const T: Sized
    T: ?Concrete + ?Sized ⇒ *mut T: Sized

Phantom types:

    T: ?Concrete + ?Sized ⇒ PhantomData<T>: Sized

References have noticeably different behavior, largely due to the existence of fat pointers on DSTs.  Since the size of a reference depends on whether the referent is `Sized`, we only have:

    T: ?Concrete + Sized ⇒ &'a T: Sized
    T: ?Concrete + Sized ⇒ &'a mut T: Sized
    T: ?Concrete + Sized ⇒ &'a [T]: Sized
    T: ?Concrete + Sized ⇒ &'a mut [T]: Sized
    &'a str: Sized
    &'a mut str: Sized

Note that the 3rd and 4th rules are not specializations because `[T]: !Sized`.

In general, parametric containers such as `Vec<T>` ought to expect `T: ?Concrete`.

---

This is ([not a new idea](https://github.com/nox/rust-rfcs/blob/master/text/1238-nonparametric-dropck.md#parametricity-via-some--bound)), by the way.  It is also similar to the mechanism Haskell uses to enforce parametricity.  Recall the example alluded to earlier:

~~~hs
{-# LANGUAGE RankNTypes #-}

data Foo t = Foo

withFoo :: (forall s . Foo s -> r) -> r
withFoo f = f Foo
~~~

This compiles, even though at no point was the compiler told what `s` should be.  The compiler generated a fictitious type for `s`.

OTOH, this does not work:

~~~hs
withFoo :: (forall s . Show s => Foo s -> r) -> r
withFoo f = f Foo
~~~

The compiler doesn’t know which `s` to pick, since they could lead to different behavior!  The compiler only allows `s` to be instantiated fictitiously if `s` remains parametric: either because there is a blanket implementation without specializations (or without “overlapping instances” as Haskell calls them) or because it is totally unconstrained.

## Universally quantified function pointers

Is it possible to reify a generic function as a universally quantified function pointer?

In general, no.  But if `T: ?Concrete + ?Sized` then no information about `T` can leak into the function, so in that particular case, it’s safe to store a function pointer to it.

~~~rust
struct Foo<T: ?Concrete + ?Sized>(PhantomData<T>);
struct Bar<T: ?Concrete + ?Sized>(PhantomData<T>);

type AnyFooBar = for<T: ?Concrete + ?Sized> fn(Foo<T>) -> Bar<T>;

fn make() -> AnyFooBar {
    |Foo(p)| Bar(p)
}

fn use<T: ?Concrete + ?Sized>(f: AnyFooBar, foo: Foo<T>) -> Bar<T> {
    f(foo)
}
~~~

Such a generic only needs to be monomorphized once (with any dummy `T` parameter), and the pointer that gets stored would simply point to that.

More generally, this works for any arbitrary type:

~~~rust
struct Blah<T: ?Concrete + ?Sized>(PhantomData<T>);

type AnyBlah = for<T: ?Concrete + ?Sized> Blah<T>;

fn make() -> AnyBlah {
    Blah(PhantomData)
}

fn use<T: ?Concrete + ?Sized>(x: AnyBlah) -> Blah<T> {
    x
}
~~~

The memory representation of `Blah<T>` cannot possibly depend on `T`, because then `T` would not be parametric.  There are very few kinds of `Blah` types that go there:

  - Function types, as described earlier.
  - Phantom types (e.g. `Phantom<*mut T>`).  Note that this only works if the data type’s constructor is public.

## Existentially quantified data types

As far as implementation strategy goes, existentially quantified types are similar to universally quantified types:

~~~rust
struct Blah<T: ?Concrete + ?Sized>(PhantomData<T>);

type SomeBlah = impl<T: ?Concrete + ?Sized> Blah<T>;

fn make() -> SomeBlah {
    impl(Blah(PhantomData)) // T is instantiated fictitiously here
}

fn use<R>(x: SomeBlah, f: for<T: ?Concrete + ?Sized> fn(Blah<T>) -> R) -> R {
    match x {
        impl(blah) => f(blah)
    }
}
~~~
