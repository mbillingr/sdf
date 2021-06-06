Exercise 3.13: Trie rules
=========================

a. Does this make any change to the dependency order?
----------------------------------------------------------

No, I don't think so. We select the first full path that matches. It should not
matter that we reuse the prefix of existing paths.


b. What characteristics of the predicates could lead to multiple handlers?
--------------------------------------------------------------------------

When they overlap.
If the predicates clearly separate the universe (such as negative and positive
numbers, or numbers and symbols) the applicability is unique.
But if we have overlapping predicates (such as negative and even numbers)
multiple predicates might be true, and thus multiple handlers might apply.


c. Are there any such situations in the generic arithmetic code?
----------------------------------------------------------------

This depends on how the generic arithmetic is constructed. There may be overlaps
in functions and symbols or in functions and vectors.
