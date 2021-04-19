Chapter 2
=========

I planned to follow along using different programming languages.
After starting out with Rust, Lua, Python and, of course, Scheme I find that
Rust and Lua are not ideal for the first part (the combinator language).

Rust does not have a notion of variable arity or multiple return values but its
type system automatically makes sure that only valid arguments can be passed
between functions. While it's possible to implement a dynamic value system and
function call protocol, one would lose many of Rust's advantages while writing
rather non-idiomatic code. Rust's static type system requires most likely a
different approach to combinators than the one taken in the book.

Lua is in spirit relatively close to Scheme (it guarantees tail calls!), and it
supports even variable argument functions and multiple return values. However,
it lacks introspection into function arity, which the combinators rely upon.

This leaves Scheme and Python in the race. Python does not have multiple return
values, but this can be worked around I think...  

Exercise 2.1
------------
- [Scheme implementation](https://github.com/mbillingr/sdf/commit/55305f7499b21264e12fd37c97bad0d8fffb5324)
- [Python implementation](https://github.com/mbillingr/sdf/commit/1aa44e739f436208d78350ed55ce76d2297fe21f)

Exercise 2.2
------------
- [Scheme and Python](https://github.com/mbillingr/sdf/commit/d3080e0d3ebdb8aa7dab5f9cce4e08991390072f)
 
