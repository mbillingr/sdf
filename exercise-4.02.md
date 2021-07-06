
- a. The commutative law aggregates numbers to the beginning of the expression,
     where they can be numerically combined.
- b. Without such an ordering we would have to look for purely numerical
     subexpressions in all operators.
     The simplification rules would need to have the following form
     `(+ (?? a) (? x) (?? b) (? y) (?? c))`
     to find two numbers anywhere within a sequence. This would involve a lot of
     backtracking. Repeatedly. Until all numbers were combined.
