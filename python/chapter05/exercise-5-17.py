"""
## Constraints
- Ben is opposite Eva
- The man at Alyssa's right has a stronger hand than Lem has.
- The man at Eva's right has a stronger hand than Ben has.
- The man at Ben's right has a stronger hand than Cy has.
- The man at Ben's right has a stronger hand than Eva has.
- The woman at Lem's right has a stronger hand than Cy has.
- The woman at Cy's right has a stronger hand than Luis has.

## Variant
- The man at Ben's right has a stronger hand than Cy has.
becomes
- The man on Ben's right is not Cy.
"""
from cps_interpreter import repl, eval_str

if __name__ == '__main__':
    with open('exercise-5-17.scm') as fd:
        eval_str(fd.read())
    repl()
