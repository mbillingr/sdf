Exercise 2.1
------------

### The Plan

1. Add an arity abstraction over n-args so that combinators don't have to do
   the arithmetic of checking and propagating arguments.
2. Replace n-args with a two-valued arity:the minimum and optional maximum
   number of args.
3. In spread-combine, at least one of the functions f and g must have a fixed
   arity so that the resulting arity can be computed.
