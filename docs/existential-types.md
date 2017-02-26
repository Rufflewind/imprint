# Existential types

A “fake” existential type is provided through `Exists`.  However, there are some gotchas with how it could be used, which mainly has to do with whether we are allowed to transmute from `F<'x>` to `F<'static>` (the underlying lifetime-erased representation).

There are three crucial properties required for a sane implementation of existential types.

  - The most important thing is that the lifetime parameter `'x` must be *fictitious*!  Otherwise, you can easily use `Exists` to smuggle objects well past their lifetimes.

  - The runtime representation between `F<'x>` and `F<'y>` must be completely *compatible* for all `'x` and `'y`.  This is akin to Leibniz equality, but we *do not* require the types to have identical behavior in the program.  We just need their memory representation to be compatible in the `transmute`-sense.

    Now, we know that this is always true for lifetimes, but if we ever get to implement one over a type parameter then this becomes important.  Some sort of [`Coercible`](https://github.com/rust-lang/rfcs/pull/91) trait would be useful here.

  - We also need *parametricity* in `F<'x>` over the variable `'x`.  So far, the `for<'a>` bound ensures parametricity because Rust does not yet allow negative bounds or specializations.  But either of these two features are added, then this requirement would have to be enforced somehow – it would be preferable to have some way to declaring this intent through something other than `unsafe` traits.

    You might say: "lifetimes are always going to be parametric because of how they work in Rust" and while that's true, enforcing parametricity in types is *also* useful, since at some point, we want to move away from phantom lifetimes and use some sort of phantom types.  Obviously very far off, but if we do hit that point we'll need to enforce parametricity in the phantom type parameter!

I don't think there is a way to encode the first property directly in Rust, so the best I can do so far is to use an unsafe trait to indicate its requirement.
