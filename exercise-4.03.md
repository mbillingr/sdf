
I have not yet looked at how the rule system is implemented. However it should
be possible to simply pre-sort the operator argument subexpressions before
applying the simplification rules. The commutative rule then becomes O(n).

We could optimize further by removing the commutative rule because it will be
never applied if the subexpressions are sorted.
I'm not sure if that is such a good idea from a design perspective because it
would make the rule implicit in the algorithm implementation.
