# Type-level values

One of the limitations of type level programming in the C++ style is that all type-level values must be known at compile time.

If you *don't* know the values at compile time, then life is harder.  The purpose of this library is to provide a basic framework to handle the latter.

One of the most powerful features in Rust is that of *higher-ranked trait bounds*.  By repurposing lifetimes to mean something else, we can use the lifetime as a marker that identifies a piece of data.  (Unfortunately, this also results in some really terrible error messages.)  In some sense, the marker acts as an abstract representation of the variable's underlying value, even though we have no idea what that value is.

(TODO: finish this)
