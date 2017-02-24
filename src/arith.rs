//! As with `Val`, the soundness and usefulness of these traits rely on the
//! type contained.  In particular, in order to enable sensible use of these
//! relations:
//!
//! - The promoted value must be a singleton type.  (This part is automatic
//! - For the demoted type:
//!     - `Eq` must be an equivalence relation, or `PartialEq` must be a
//!       partial equivalence relation.
//!     - All arithmetic properties are expected to be invariant under the
//!       `Eq` or `PartialEq` relation of the demoted type.
//!
//! We do not check any of these properties here.  As a result, types that
//! rely on these properties to unsafe behavior under a safe interface must
//! use either trustworthy types or restrict using an unsafe trait.
//!
//! See [`Val`](../struct.Val.html#properties) for more info.

use std::fmt;
use std::marker::PhantomData;
use num::Integer;
use super::{PhantomInvariantType, TyEq, Val, Value, imprint};

macro_rules! impl_all1 {
    ( $name:ident ) => {
        impl<X: ?Sized> $name<X> {
            pub unsafe fn conjure() -> Self { $name(PhantomData) }
        }

        // shut up clippy: we don't want Clone constraints on X
        #[cfg_attr(feature = "cargo-clippy", allow(expl_impl_clone_on_copy))]
        impl<X: ?Sized> Clone for $name<X> {
            fn clone(&self) -> Self { *self }
        }

        impl<X: ?Sized> Copy for $name<X> { }

        impl<X: ?Sized> fmt::Debug for $name<X> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str(stringify!($name))
            }
        }
    }
}

macro_rules! impl_all2 {
    ( $name:ident ) => {
        impl<X: ?Sized, Y: ?Sized> $name<X, Y> {
            pub unsafe fn conjure() -> Self { $name(PhantomData, PhantomData) }
        }

        // shut up clippy: we don't want Clone constraints on X or Y
        #[cfg_attr(feature = "cargo-clippy", allow(expl_impl_clone_on_copy))]
        impl<X: ?Sized, Y: ?Sized> Clone for $name<X, Y> {
            fn clone(&self) -> Self { *self }
        }

        impl<X: ?Sized, Y: ?Sized> Copy for $name<X, Y> { }

        impl<X: ?Sized, Y: ?Sized> fmt::Debug for $name<X, Y> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str(stringify!($name))
            }
        }
    }
}

/// Negation.
pub struct Not<P: ?Sized>(PhantomInvariantType<P>);

impl<P> Not<P> {
    pub fn absurd(_: P) -> ! { panic!() }
}

impl_all1!(Not);

/// Equal to.
///
/// This is separate from `TyEq` because `TyEq` only holds for `Val`, whereas
/// `Equal` can be used in more general contexts.
pub struct Equal<X: ?Sized, Y: ?Sized>(
    PhantomInvariantType<X>,
    PhantomInvariantType<Y>,
);

impl<X: ?Sized, Y: ?Sized> Equal<X, Y> {
    pub fn refl() -> Self {
        unsafe { Equal::conjure() }
    }

    pub fn sym(self) -> Equal<Y, X> {
        unsafe { Equal::conjure() }
    }

    pub fn trans<Z: ?Sized>(self, _: Equal<Y, Z>) -> Equal<X, Z> {
        unsafe { Equal::conjure() }
    }
}

impl_all2!(Equal);

impl<'x, 'y, T> Equal<Val<'x, T>, Val<'y, T>> {
    /// If two `Val`s are equal, then their types are equal too.
    pub fn into_ty_eq(self) -> TyEq<Val<'x, T>, Val<'y, T>> {
        use std::mem::transmute;
        unsafe { transmute(TyEq::<Val<'x, T>, Val<'x, T>>::refl()) }
    }
}

/// Less than.
pub struct Less<X: ?Sized, Y: ?Sized>(
    PhantomInvariantType<X>,
    PhantomInvariantType<Y>,
);

impl<X: ?Sized, Y: ?Sized> Less<X, Y> {
    /// `(X < Y, X = Z) -> Z < Y`
    pub fn lsubst<Z: ?Sized>(self, _: Equal<X, Z>) -> Less<Z, Y> {
        unsafe { Less::conjure() }
    }

    /// `(X < Y, Y = Z) -> X < Z`
    pub fn rsubst<Z: ?Sized>(self, _: Equal<Y, Z>) -> Less<X, Z> {
        unsafe { Less::conjure() }
    }

    /// `(X < Y, Y < Z) -> X < Z`
    pub fn comp<Z: ?Sized>(self, _: Less<Y, Z>) -> Less<X, Z> {
        unsafe { Less::conjure() }
    }

    /// `(X < Y, Y <= Z) -> X < Z`
    pub fn rcomp_le<Z: ?Sized>(self, _: LessEqual<Y, Z>) -> Less<X, Z> {
        unsafe { Less::conjure() }
    }

    /// `(X < Y, Z <= X) -> Z < Y`
    pub fn lcomp_le<Z: ?Sized>(self, _: LessEqual<Z, X>) -> Less<Z, Y> {
        unsafe { Less::conjure() }
    }
}

impl_all2!(Less);

/// Less than or equal to (but we don't know which is the case).
pub struct LessEqual<X: ?Sized, Y: ?Sized>(
    PhantomInvariantType<X>,
    PhantomInvariantType<Y>,
);

impl<X: ?Sized, Y: ?Sized> From<Less<X, Y>> for LessEqual<X, Y> {
    fn from(_: Less<X, Y>) -> Self {
        unsafe { LessEqual::conjure() }
    }
}

impl<X: ?Sized, Y: ?Sized> From<Equal<X, Y>> for LessEqual<X, Y> {
    fn from(_: Equal<X, Y>) -> Self {
        unsafe { LessEqual::conjure() }
    }
}

impl<X: ?Sized, Y: ?Sized> LessEqual<X, Y> {
    /// `(X <= Y, X = Z) -> Z <= Y`
    pub fn lsubst<Z: ?Sized>(self, _: Equal<X, Z>) -> LessEqual<Z, Y> {
        unsafe { LessEqual::conjure() }
    }

    /// `(X <= Y, Y = Z) -> X <= Z`
    pub fn rsubst<Z: ?Sized>(self, _: Equal<Y, Z>) -> LessEqual<X, Z> {
        unsafe { LessEqual::conjure() }
    }

    /// `(X <= Y, Y <= Z) -> X <= Z`
    pub fn comp<Z: ?Sized>(self, _: LessEqual<Y, Z>) -> LessEqual<X, Z> {
        unsafe { LessEqual::conjure() }
    }
}

impl_all2!(LessEqual);

/// Greater than.
pub type Greater<X, Y> = Less<Y, X>;

/// Greater than or equal to.
pub type GreaterEqual<X, Y> = LessEqual<Y, X>;

/// Compare two values for partial equality.
pub fn partial_equal<'a, X, Y, T>(x: &'a X, y: &'a Y) -> Option<Equal<X, Y>>
    where &'a X: Value<Value=T>,
          &'a Y: Value<Value=T>,
          T: PartialEq {
    if &x.value() == &y.value() {
        Some(unsafe { Equal::conjure() })
    } else {
        None
    }
}

/// Compare two values for partial equality.
pub fn partial_not_equal<'a, X, Y, T>(x: &'a X, y: &'a Y)
                                      -> Option<Not<Equal<X, Y>>>
    where &'a X: Value<Value=T>,
          &'a Y: Value<Value=T>,
          T: PartialEq {
    if &x.value() != &y.value() {
        Some(unsafe { Not::conjure() })
    } else {
        None
    }
}

/// Compare two values for equality.
pub fn equal<'a, X, Y, T>(x: &'a X, y: &'a Y)
                          -> Result<Equal<X, Y>, Not<Equal<X, Y>>>
    where &'a X: Value<Value=T>,
          &'a Y: Value<Value=T>,
          T: Eq {
    if &x.value() == &y.value() {
        Ok(unsafe { Equal::conjure() })
    } else {
        Err(unsafe { Not::conjure() })
    }
}

/// Compare two values for partial ordering.
pub fn partial_compare<'a, X, Y, T>(x: &'a X, y: &'a Y)
                                    -> Option<Result<Less<X, Y>,
                                                     Result<Greater<X, Y>,
                                                            Equal<X, Y>>>>
    where &'a X: Value<Value=T>,
          &'a Y: Value<Value=T>,
          T: PartialOrd {
    use std::cmp::Ordering;
    match PartialOrd::partial_cmp(&x.value(), &y.value()) {
        None => None,
        Some(ordering) => Some(match ordering {
            Ordering::Less => Ok(unsafe { Less::conjure() }),
            Ordering::Equal => Err(Err(unsafe { Equal::conjure() })),
            Ordering::Greater => Err(Ok(unsafe { Less::conjure() })),
        }),
    }
}

/// Compare two values for ordering.
pub fn compare<'a, X, Y, T>(x: &'a X, y: &'a Y)
                            -> Result<Less<X, Y>,
                                      Result<Greater<X, Y>,
                                             Equal<X, Y>>>
    where &'a X: Value<Value=T>,
          &'a Y: Value<Value=T>,
          T: Ord {
    use std::cmp::Ordering;
    match Ord::cmp(&x.value(), &y.value()) {
        Ordering::Less => Ok(unsafe { Less::conjure() }),
        Ordering::Equal => Err(Err(unsafe { Equal::conjure() })),
        Ordering::Greater => Err(Ok(unsafe { Less::conjure() })),
    }
}

pub fn succ<'x, 'y, I, F, R>(x: &Val<'x, I>,
                             _: Less<Val<'x, I>, Val<'y, I>>,
                             callback: F) -> R
    where F: for<'z> FnOnce(Val<'z, I>,
                            Less<Val<'x, I>, Val<'z, I>>,
                            LessEqual<Val<'z, I>, Val<'y, I>>) -> R,
          I: Clone + Integer {
    imprint(x.value().clone() + I::one(), |z| {
        callback(z,
                 unsafe { Less::conjure() },
                 unsafe { LessEqual::conjure() })
    })
}

#[cfg(test)]
mod tests {
    use super::super::*;
    use super::*;

    #[test]
    fn it_works() {
        imprint(1, |one| { imprint(2, |two| { imprint(1, |one_| {
            assert!(partial_compare(&one, &two).is_some());
            assert!(compare(&one, &two).is_ok());
            assert!(compare(&two, &one).unwrap_err().is_ok());
            assert!(equal(&two, &one).is_err());
            assert!(partial_equal(&two, &one).is_none());
            assert!(compare(&one, &one_).unwrap_err().is_err());
            assert!(equal(&one, &one_).is_ok());
            assert!(partial_equal(&one, &one_).is_some());
        }) }) });
    }
}
