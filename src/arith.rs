use std::fmt;
use std::marker::PhantomData;
use std::ops::Deref;
use super::{PhantomInvariantData, TyEq, Val};

macro_rules! impl_all {
    ( $name:ident ) => {
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

/// Equal to.
pub struct Equal<X: ?Sized, Y: ?Sized>(
    PhantomInvariantData<X>,
    PhantomInvariantData<Y>,
);
impl_all!(Equal);

impl<'x, 'y, T> Equal<Val<'x, T>, Val<'y, T>> {
    /// If two `Val`s are equal, then their types are equal too.
    pub fn into_ty_eq(self) -> TyEq<Val<'x, T>, Val<'y, T>> {
        use std::mem::transmute;
        unsafe { transmute(TyEq::<Val<'x, T>, Val<'x, T>>::refl()) }
    }
}

/// Not equal to.
pub struct NotEqual<X: ?Sized, Y: ?Sized>(
    PhantomInvariantData<X>,
    PhantomInvariantData<Y>,
);
impl_all!(NotEqual);

/// Less than.
pub struct Less<X: ?Sized, Y: ?Sized>(
    PhantomInvariantData<X>,
    PhantomInvariantData<Y>,
);
impl_all!(Less);

/// Greater than.
pub struct Greater<X: ?Sized, Y: ?Sized>(
    PhantomInvariantData<X>,
    PhantomInvariantData<Y>,
);
impl_all!(Greater);

/// Less than or equal to.
pub type LessEqual<X, Y> = Result<Less<X, Y>, Equal<X, Y>>;

/// Greater than or equal to.
pub type GreaterEqual<X, Y> = Result<Greater<X, Y>, Equal<X, Y>>;

/// Compare two values for partial equality.
pub fn partial_equal<X, Y, T>(x: &X, y: &Y) -> Option<Equal<X, Y>>
    where X: Deref<Target=T>, Y: Deref<Target=T>, T: PartialEq + ?Sized {
    if &**x == &**y {
        Some(Equal(PhantomData, PhantomData))
    } else {
        None
    }
}

/// Compare two values for equality.
pub fn equal<X, Y, T>(x: &X, y: &Y) -> Result<Equal<X, Y>, NotEqual<X, Y>>
    where X: Deref<Target=T>, Y: Deref<Target=T>, T: Eq + ?Sized {
    if &**x == &**y {
        Ok(Equal(PhantomData, PhantomData))
    } else {
        Err(NotEqual(PhantomData, PhantomData))
    }
}

/// Compare two values for partial ordering.
pub fn partial_compare<X, Y, T>(x: &X, y: &Y)
                                -> Option<Result<Less<X, Y>,
                                                 GreaterEqual<X, Y>>>
    where X: Deref<Target=T>, Y: Deref<Target=T>, T: Ord + ?Sized {
    use std::cmp::Ordering;
    match PartialOrd::partial_cmp(&**x, &**y) {
        None => None,
        Some(ordering) => Some(match ordering {
            Ordering::Less => Ok(Less(PhantomData, PhantomData)),
            Ordering::Equal => Err(Err(Equal(PhantomData, PhantomData))),
            Ordering::Greater => Err(Ok(Greater(PhantomData, PhantomData))),
        }),
    }
}

/// Compare two values for ordering.
pub fn compare<X, Y, T>(x: &X, y: &Y) -> Result<Less<X, Y>, GreaterEqual<X, Y>>
    where X: Deref<Target=T>, Y: Deref<Target=T>, T: Ord + ?Sized {
    use std::cmp::Ordering;
    match Ord::cmp(&**x, &**y) {
        Ordering::Less => Ok(Less(PhantomData, PhantomData)),
        Ordering::Equal => Err(Err(Equal(PhantomData, PhantomData))),
        Ordering::Greater => Err(Ok(Greater(PhantomData, PhantomData))),
    }
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
