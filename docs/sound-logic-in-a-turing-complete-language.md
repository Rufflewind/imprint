# “Sound” logic in a Turing-complete language

The problem with Turing-complete languages is that we can do things like this:

    fn proof() -> RiemannHypothesis {
        loop { }
    }

or this:

    fn proof() -> RiemannHypothesis {
        panic!("I did it!")
    }

So in this sense, we can prove anything we want in this language.

That being said, there’s a big difference between:

  - functions that stall or panic (non-productive) and
  - functions that exit cleanly (productive).

The key difference is that non-productive functions will never move on to the next stage.  They just give up.  Productive functions OTOH will.

Therefore, *to some extent we can trust the proofs given to us*: if the proof is inconsistent (non-productive) then we'll never see the proof, because the program will have already crashed or hung forever.

This assumes the language has a *safe* type system and we hold ourselves to only the safe subset.  In C++, there’s no demarcation between safe and unsafe code, so you can probably get away with just returning an uninitialized object or something (never mind the fact that C++ lacks the expressiveness to even do logic properly due to lack of higher-rank types).

However, the “to some extent” part is important.  Not all proofs are equally trustworthy in this language.  In fact, *one must fully evaluate a proof in order to trust it*.  This means if we have plain old data object, then the proof is trustworthy.  But if we have a function or equivalent, then the proof is spurious, *unless* you evaluate that proof at the point it’s needed.

## Negations

One common situation where this problem occurs is in that of *negations*.  In type theory, a negation `¬A` is normally represented by a function `A → ⊥`.  But in an inconsistent language `⊥` is *ubiquitous*!  We can trivially construct the negation of anything:

    fn negation_proof(Equal<X, Y>) -> ! {
        panic!("Trust me: X is definitely not equal to Y")
    }

This can be problematic, depending on what you do with such a proof.  The “correct” way to deal with it is this:

    match equal(x, y) {
        Ok(equal) => { negation_proof(equal) }
        Err(not_equal) => { … }
    }

But then what's the point?  We still have no idea if the `negation_proof` was valid, because the proof always crashes.

Moreover, we just re-tested `x` and `y` for equality … again, so what's the whole point of having a proof if we gotta redo all the work?  This just seems like a fancy way to pass in an error message!

It would be nice if we somehow *depend* on `negation_proof` being valid and then use that to actually do something productive, instead of crashing no matter what.  But I don’t think it’s possible this way.

Therefore, the only way we can handle this is to make negation proofs unsafe to construct, by defining our own custom data type for it `Not⟨A⟩`, which I will call the **strict negation**.  In contrast, `A → ⊥` is a **lazy negation**.  As earlier, lazy proofs are never trustworthy unless fully evaluated.  From `Not⟨A⟩` we can always derive `A → ⊥` soundly, but not the other way around.

## Double-negation elimination

    (A → ⊥) → ⊥) → A

Double-negation elimination only works in a language that supports setjmp/longjmp mechanics.  In Rust, we could conceivably do this using `panic` and `catch_unwind` but that sounds awfully hacky (and possibly dangerous?).  Or we could spawn a thread and catch its panic, which is really inefficient but works.

~~~rust
// cargo-deps: snowflake, void
extern crate snowflake;
extern crate void;

use std::any::Any;
use std::marker::PhantomData;
use std::panic::{self, UnwindSafe};
use snowflake::ProcessUniqueId;
use void::{ResultVoidErrExt, Void};

pub struct Escape<T>(PhantomData<*mut T>, ProcessUniqueId);

struct Escaping(ProcessUniqueId, Box<Any + Send>);

impl<T: Send + 'static> Escape<T> {
    pub fn absurd(self, x: T) -> Void {
        panic!(Escaping(self.1, Box::new(x)))
    }
}

/// Silence the panic messages to stderr if the payload is Escaping.
fn install_panic_handler() {
    use std::sync;
    static INSTALLER: sync::Once = sync::ONCE_INIT;
    INSTALLER.call_once(|| {
        let p = panic::take_hook();
        panic::set_hook(Box::new(move |info| {
            if !info.payload().is::<Escaping>() {
                p(info)
            }
        }));
    })
}

pub fn dne<T, F>(callback: F) -> T
    where T: Send + 'static,
          F: FnOnce(Escape<T>) -> Void + UnwindSafe {
    install_panic_handler();
    let id = ProcessUniqueId::new();
    #[allow(unreachable_patterns)]
    match panic::catch_unwind(|| {
        callback(Escape(PhantomData, id))
    }).void_unwrap_err().downcast::<Escaping>() {
        Ok(x) => if x.0 == id {
            *x.1.downcast::<T>().unwrap()
        } else {
            panic::resume_unwind(x)
        },
        Err(e) => panic::resume_unwind(e),
    }
}

fn main() {
    println!("got: {:?}", dne(|escape| {
        escape.absurd(42i64)
    }));
}
~~~

## Using a dummy type?

Could we perhaps use a dummy type instead of `⊥`?  If so, we could verify that `⊥` was indeed constructed legitimately rather than through a panic.  But that still requires constructing `A`!
