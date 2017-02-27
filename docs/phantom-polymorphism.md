# Phantom polymorphism

Ideally, it would be great to not have to use lifetimes for this library -- the only reason for that is to get higher-ranked trait bounds, which can quantify over lifetimes.  In other ways lifetimes are not as rich as types (you can't name arbitrary lifetimes or compose new lifetimes from existing ones).  Plus, lifetimes tend to linger as a trait bound even after erasing them (with e.g. trait objects).  Moreover, abusing lifetimes leads to awful compile errors.

So, what if we [amended the type system to support higher-ranked trait bounds with *type* parameters](https://github.com/rust-lang/rfcs/issues/1481)?  Better yet, what if they are allowed on ordinary types, not just trait bounds?

## Universally quantified types

Consider what a simple universally-quantified type would look like

~~~rust
let x: for<T> F<T>;
~~~

where `F` is some arbitrary type function.

### Requirements

A natural question to ask is how such an object would be represented at run time.  In Rust, every concrete type must have a concrete memory representation.  It follows that an object of type `for<T> F<T>` must have the same representation for every `T`: we say `F<T>` must be “representationally independent” of `T` (another way of saying that `T` must be phantom in `F<T>`).

An obvious example would be something like `PhantomData<T>` (and anything built exclusively using that).  Another potential example would be `*mut T` or `*const T`.

An obvious counter-example would be `T` itself, since the representation of `T` clearly depends on itself!

A somewhat nonobvious example is that of `&T` (ignoring lifetimes for now).  The reason why `T` is not phantom in `&T` is because of dynamically sized types: `&[i32]` are fat pointers, whereas references like `&i32` are not.  This is kind of annoying.  We can narrow things down using the rule:

> `T` is phantom in `&T` if `T: Sized`.

If `F` is a *function*-ish type, as it often will be (universal types are often just generic functions reified as objects), then we have to further ensure that the behavior of the function is independent of the type `T`.  This in turn is where *parametricity* comes in.  We need the generic function to be completely parametric!

There is no way to enforce this in Rust.  Naively you’d think that `T: ?Sized` means that there's nothing you can do with `T` except to pass it around, but that is false in Rust because it supports blanket `impl`s.

The lack of parametricity is worrisome.  One could perhaps add an opt-out trait `?Nonparametric` to fix this?

### Implementation

#### Construction

~~~rust
fn new<F>(for<T> F<T>) -> Forall<F>;
~~~

To construct a universal type, we simply instantiate it with `T = ()` and store that.  The hard part is figuring out to *accept* a generic!  We can use traits here:

~~~rust
// not legal in Rust today
pub trait Generic {
    type F: for<T> TyFn<T> where for<T> <F as TyFn<T>>::Output: Sized;
    fn call_generic<T: ?Sized + ?Nonparametric>(self) -> <Self::F as TyFn<T>>::Output;
}
~~~

(Turns out this trait is pretty much isomorphic to a universal type.)

#### Elimination

~~~rust
fn get<F, T>(Forall<F>) -> F<T>;
~~~

To use a universal type, we take the instantiated object and then use `transmute` to convert `F<()>` back into an arbitrary `F<T>`.  This is why we need representational independence!

At some point, when we use the universal type, we need to instantiate it by filling in the type parameters.

### Failed attempt

I don't think it's possible to do this in Rust today.  Below is a failed attempt.

The problem I ran into was that the compiler was unable to infer `<PtrConstF as TyFn<T>>::Output: Sized`, even though it's literally right there in the `where` clause, and could be easily inferred from the implementation of `TyFn<T> for PtrConstF`.  There are probably other issues as well that prevent an implementation, but I didn't bother to continue (for now).

~~~rust
pub trait Generic {
    type F: ?Sized;
    fn instantiate<T: ?Sized>(self) -> <Self::F as TyFn<T>>::Output
        // not exactly what we want...
        // we really want these bounds directly on F itself
        where Self::F: TyFn<T>,
              <Self::F as TyFn<T>>::Output: Sized;
}

pub struct PtrConstF;

impl<T: ?Sized> TyFn<T> for PtrConstF {
    type Output = *const T;
}

pub struct PtrConstIdFn<T: ?Sized>(PhantomInvariantType<T>);

impl<T: ?Sized> PtrConstIdFn<T> {
    pub fn call(x: *mut T) -> *mut T {
        x
    }
}

pub struct PtrConstIdGeneric;

impl Generic for PtrConstIdGeneric {
    type F = PtrConstF;
    fn instantiate<T: ?Sized>(self) -> <Self::F as TyFn<T>>::Output
        // not exactly what we want...
        // we really want F: for<T> TyFn<T>
        where Self::F: TyFn<T>,
              <PtrConstF as TyFn<T>>::Output: Sized
    {
        PtrConstIdFn(PhantomData)
    }
}

pub struct Forall<F: TyFn<()>>(
    <F as TyFn<()>>::Output,
);
~~~

## Existentially quantified types

As far as implementation strategy goes, existentially quantified types are similar to universally quantified types.

Like before, objects of type `F<T>` must be representationally independent of `T` in order for this to work.  We also have to make sure the callback cannot inspect `T` in any way, so it must necessarily be parametric.

### Construction

~~~rust
fn new<F, T>(F<T>) -> Exists<F>;
~~~

When you construct it, you grab an object `F<T>` and then transmute it into `F<()>` so you can store it.

### Elimination

~~~rust
fn with<F, R>(Exists<F>, G) -> R
  where for<T> G: FnOnce(F<T>) -> R;
~~~

Later on, when you inspect the object, you have to do it within a `for<U: ?Sized + ?Nonparametric> FnOnce(F<U>) -> R` callback.

### Partial example

This has been implemented for `F<'a>` (with a lifetime parameter) already in `src/lib.rs`.
