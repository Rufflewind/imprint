# Safe indexing

Some generalizations we could do:

  - allow arbitrary "Key"s with "Key -> usize" functions
  - allow multidimensional indices (see: "Key" idea)
  - allow indices to have some part that varies by slice
    (e.g. the so-called "leading dimension" in matrices need to be stored
    with the slice!)
  - allow both checked (with len) indexing and unchecked (len-free) indexing
  - composition of "Key -> Key" functions:
    (X :-> Y, Y :-> Z) -> (X :-> Z)

Other future improvements:

  - Use unsized types for better ergonomics.
    (For example, right now, I have do duplicate the implementation of
    Index and IndexMut in BoxedSl)
  - Store proofs within Ix using Exists, whenever the ICEs get fixed.
  - Could use a more general "Subtype" proof for downcasts
    (used for e.g. Ix::check)
  - Add support for *compile-time* integers in `typenum` (note: for
    compile-time ones you sometimes want to store them "on the stack")

~~~rust
[&I] -> &T
[&I] -> &mut T

IdxFn ~ (&Key -> Option<impl Borrow<Idx>>)
Store ~ (&Idx -> Option<&'self mut? Val>)

(A -o> B) -> (B -o> C) -> (A -o> C)
(A -m> B) -> (B -m> C) -> (A -m> C)
(A -> B) -> (B -> C) -> (A -> C)

pub unsafe trait Get<Idx: ?Sized>: Index<Idx> {
    fn get(&self, &Idx) -> Option<&Self::Output>;
}

pub unsafe trait GetMut<Idx: ?Sized>: Get<Idx> + IndexMut<Idx> {
    fn get_mut(&self, &Idx) -> Option<&mut Self::Output>;
}

pub struct Mapping<I: IdxFn, S: Get<Idx>> {
    indexer: I,
    store: S,
}

pub unsafe trait Indicial: Deref { }

unsafe impl<'x, I: PrimInt> Indicial for Val<'x, I> { }
~~~
