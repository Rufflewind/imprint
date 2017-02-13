use std::marker::PhantomData;
use std::ops::{Deref, DerefMut, Index, IndexMut};
use num::Zero;
use num_iter::{Range, range};
use super::*;
use arith::{Less, GreaterEqual, compare};

/// Represents a value less than `'l`.
///
/// Equivalent to: `exists<'i> (Less<Val<'i, usize>, Val<'l, usize>>, Val<'i,
/// usize>)`.
///
/// In the future, `Ix` will be changed to a more transparent implementation
/// using `Exists` whenever the ICEs get fixed.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ix<'l> {
    len: PhantomData<Val<'l, usize>>,
    inner: usize,
}

impl<'l> Ix<'l> {
    pub fn new<'i>(index: Val<'i, usize>,
                   _: Less<Val<'i, usize>, Val<'l, usize>>)
                   -> Self {
        unsafe { Self::from_raw(index.into_inner()) }
    }

    pub fn try_new(index: usize, len: Val<'l, usize>) -> Option<Self> {
        imprint(index, |i| {
            match compare(&i, &len) {
                Ok(lt) => Some(Self::new(i, lt)),
                Err(_) => None,
            }
        })
    }

    pub fn check<'m>(self, _: GreaterEqual<Val<'m, usize>, Val<'l, usize>>)
                     -> Ix<'m> {
        unsafe { Ix::from_raw(self.into_inner()) }
    }

    pub unsafe fn from_raw(index: usize) -> Self {
        Ix { len: PhantomData, inner: index }
    }
}

impl<'i> fmt::Debug for Ix<'i> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("Ix(")?;
        (*self).fmt(f)?;
        f.write_str(")")
    }
}

impl<'l> AsRef<usize> for Ix<'l> {
    fn as_ref(&self) -> &usize {
        &**self
    }
}

impl<'l> Borrow<usize> for Ix<'l> {
    fn borrow(&self) -> &usize {
        &**self
    }
}

impl<'l> Deref for Ix<'l> {
    type Target = usize;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'l> IntoInner for Ix<'l> {
    fn into_inner(self) -> Self::Target {
        self.inner
    }
}

pub struct BoxedSl<'l, T> {
    len: PhantomData<Val<'l, usize>>,
    inner: Box<[T]>,
}

impl<'l, T> BoxedSl<'l, T> {
    pub fn from_boxed_slice(boxed_slice: Box<[T]>, len: Val<'l, usize>)
                            -> Result<Self, Box<[T]>> {
        if boxed_slice.len() == len.into_inner() {
            Ok(unsafe { Self::from_raw(boxed_slice) })
        } else {
            Err(boxed_slice)
        }
    }

    pub fn new(len: Val<'l, usize>, value: T) -> Self where T: Clone {
        unsafe { Self::from_raw(
            vec![value; len.into_inner()].into_boxed_slice()
        ) }
    }

    pub unsafe fn from_raw(boxed_slice: Box<[T]>) -> Self {
        BoxedSl {
            len: PhantomData,
            inner: boxed_slice,
        }
    }

    pub fn len(&self) -> Val<'l, usize> {
        unsafe { Val::known((**self).len()) }
    }

    pub fn as_slice<'a>(&'a self) -> Sl<'a, 'l, T> {
        unsafe { Sl::from_raw((**self).as_ptr()) }
    }

    pub fn as_mut_slice<'a>(&'a mut self) -> MutSl<'a, 'l, T> {
        unsafe { MutSl::from_raw((**self).as_mut_ptr()) }
    }
}

impl<'l, T> Deref for BoxedSl<'l, T> {
    type Target = Box<[T]>;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'l, T> DerefMut for BoxedSl<'l, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<'l, T> IntoInner for BoxedSl<'l, T> {
    fn into_inner(self) -> Self::Target {
        self.inner
    }
}

impl<'a, 'l, T: fmt::Debug> fmt::Debug for BoxedSl<'l, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("BoxedSl(")?;
        self.inner.fmt(f)?;
        f.write_str(")")
    }
}

impl<'l, T> Index<Ix<'l>> for BoxedSl<'l, T> {
    type Output = T;
    fn index(&self, index: Ix<'l>) -> &Self::Output {
        // can't use Sl impl because we aren't using unsized types!
        unsafe { self.inner.get_unchecked(index.into_inner()) }
    }
}

impl<'l, T> IndexMut<Ix<'l>> for BoxedSl<'l, T> {
    fn index_mut<'a>(&'a mut self, index: Ix<'l>) -> &'a mut Self::Output {
        // can't use MutSl impl because we aren't using unsized types!
        unsafe { self.inner.get_unchecked_mut(index.into_inner()) }
    }
}

#[derive(Clone, Copy)]
pub struct Sl<'a, 'l, T: 'a> {
    len: PhantomData<(Val<'l, usize>, &'a T)>,
    ptr: *const T,
}

impl<'a, 'l, T> Sl<'a, 'l, T> {
    pub fn from_slice(slice: &'a [T], len: Val<'l, usize>) -> Option<Self> {
        if slice.len() == len.into_inner() {
            Some(unsafe { Self::from_raw(slice.as_ptr()) })
        } else {
            None
        }
    }

    pub unsafe fn from_raw(ptr: *const T) -> Self {
        Sl { len: PhantomData, ptr: ptr }
    }

    pub fn as_ptr(self) -> *const T {
        self.ptr
    }

    pub fn as_slice(self, len: Val<'l, usize>) -> &'a [T] {
        use std::slice;
        unsafe { slice::from_raw_parts(self.as_ptr(), len.into_inner()) }
    }
}

impl<'a, 'l, T> fmt::Debug for Sl<'a, 'l, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("Sl(")?;
        // we can't print the contents without knowledge of its size :/
        self.ptr.fmt(f)?;
        f.write_str(")")
    }
}

impl<'a, 'l, T> Index<Ix<'l>> for Sl<'a, 'l, T> {
    type Output = T;
    fn index(&self, index: Ix<'l>) -> &Self::Output {
        unsafe { &*self.ptr.offset(index.into_inner() as isize) }
    }
}

pub struct MutSl<'a, 'l, T: 'a> {
    len: PhantomData<(Val<'l, usize>, &'a mut T)>,
    ptr: *mut T,
}

impl<'a, 'l, T> MutSl<'a, 'l, T> {
    pub fn from_slice(slice: &'a mut [T], len: Val<'l, usize>)
                      -> Option<Self> {
        if slice.len() == len.into_inner() {
            Some(unsafe { Self::from_raw(slice.as_mut_ptr()) })
        } else {
            None
        }
    }

    pub fn as_slice<'b>(&'b self, len: Val<'l, usize>) -> &'b [T] {
        use std::slice;
        unsafe { slice::from_raw_parts_mut(self.ptr, len.into_inner()) }
    }

    pub fn into_slice(self, len: Val<'l, usize>) -> &'a mut [T] {
        use std::slice;
        unsafe { slice::from_raw_parts_mut(self.ptr, len.into_inner()) }
    }

    pub unsafe fn from_raw(ptr: *mut T) -> Self {
        MutSl { len: PhantomData, ptr: ptr }
    }

    pub fn as_ptr(self) -> *const T {
        self.ptr
    }

    pub fn as_mut_ptr(self) -> *mut T {
        self.ptr
    }
}

impl<'a, 'l, T> fmt::Debug for MutSl<'a, 'l, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("MutSl(")?;
        // we can't print the contents without knowledge of its size :/
        self.ptr.fmt(f)?;
        f.write_str(")")
    }
}

impl<'a, 'l, T> Index<Ix<'l>> for MutSl<'a, 'l, T> {
    type Output = T;
    fn index(&self, index: Ix<'l>) -> &Self::Output {
        unsafe { &*self.ptr.offset(index.into_inner() as isize) }
    }
}

impl<'a, 'l, T> IndexMut<Ix<'l>> for MutSl<'a, 'l, T> {
    fn index_mut(&mut self, index: Ix<'l>) -> &mut Self::Output {
        unsafe { &mut *self.ptr.offset(index.into_inner() as isize) }
    }
}

#[derive(Clone)]
pub struct IxRange<'l> {
    len: PhantomData<Val<'l, usize>>,
    inner: Range<usize>,
}

impl<'l> IxRange<'l> {
    /// `[start .. stop)`
    pub fn new(start: usize, stop: Ix<'l>) -> Self {
        unsafe { Self::from_raw(start, stop.into_inner()) }
    }

    /// `[0 .. stop)`
    pub fn new_to(stop: Ix<'l>) -> Self {
        Self::new(Zero::zero(), stop)
    }

    /// `[start ... stop]`
    pub fn new_inclusive(start: usize, stop_inclusive: Ix<'l>) -> Self {
        unsafe { Self::from_raw(start, stop_inclusive.into_inner() + 1) }
    }

    /// `[0 .. stop]`
    pub fn new_to_inclusive(stop_inclusive: Ix<'l>) -> Self {
        Self::new_inclusive(Zero::zero(), stop_inclusive)
    }

    /// `[start .. len)`
    pub fn new_from(start: usize, len: Val<'l, usize>) -> Self {
        unsafe { Self::from_raw(start, len.into_inner()) }
    }

    /// `[0 .. len)`
    pub fn new_full(len: Val<'l, usize>) -> Self {
        Self::new_from(Zero::zero(), len)
    }

    pub unsafe fn from_raw(start: usize, stop: usize) -> Self {
        IxRange {
            len: PhantomData,
            inner: range(start, stop),
        }
    }
}

impl<'l> Deref for IxRange<'l> {
    type Target = Range<usize>;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'l> DerefMut for IxRange<'l> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<'l> Iterator for IxRange<'l> {
    type Item = Ix<'l>;

    fn next(&mut self) -> Option<Self::Item> {
        (**self).next().map(|i| unsafe { Ix::from_raw(i) })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (**self).size_hint()
    }
}

impl<'l> DoubleEndedIterator for IxRange<'l> {
    fn next_back(&mut self) -> Option<Self::Item> {
        (**self).next_back().map(|i| unsafe { Ix::from_raw(i) })
    }
}

#[cfg(test)]
mod tests {
    use super::super::*;
    use super::*;

    #[test]
    fn it_works() {
        let n_ = 42;
        imprint(n_, |n| {
            let vec: Vec<usize> = (0 .. n_).collect();
            let slice = Sl::from_slice(&vec, n).unwrap();
            let mut a = BoxedSl::new(n, 0.0);
            let mut b = BoxedSl::new(n, 0.0);
            for i in IxRange::new_full(n) {
                let i_ = *i;
                a[i] = i_ as f64 * 0.5;
                b[i] = (n_ - i_) as f64;
            }
            for i in IxRange::new_full(n) {
                let i_ = *i;
                assert_eq!(slice[i], i_);
                assert_eq!(a[i], i_ as f64 * 0.5);
                assert_eq!(b[i], (n_ - i_) as f64);
            }
        })
    }
}
