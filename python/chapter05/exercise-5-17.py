"""
- The arrangement is unique. It is still unique when relaxing the first "Ben" constraint.
  (I suspect this constraint is redundant and could be removed altogether.)

- My solution takes just a bit more than 2 minutes on a 2019 desktop PC.
  I'm not sure if I arrived at the "clever" solution the book authors mentioned because it's
  hard to compare their Scheme to my Python implementation of the interpreter.

  I tried creating all permutations of positions (without fixing Ben and Eva) which increased
  runtime to ___. Maybe the "clever" solution is to apply constraints as early as possible, before
  generating more variants? (In contrast to "straight forward"ly generating all value combinations
  up-front and then applying the constraints?
  Does that mean that my permutation-generating implementation was over-engineered?

"""
from cps_interpreter import repl, eval_str
from time import time

if __name__ == '__main__':
    with open('exercise-5-17.scm') as fd:
        start = time()
        eval_str(fd.read())
        print(f";;; {time() - start} seconds elapsed")
    repl()
