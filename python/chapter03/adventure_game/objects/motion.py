from chapter03.adventure_game.adventure_substrate.messaging import tell
from chapter03.multimethods import match_args
from chapter03.multimethods import most_specific_generic_procedure
from .container import Container
from .mobile_thing import MobileThing
from .person import Person
from .thing import Thing

generic_move = most_specific_generic_procedure("generic-move")


def take_exit(exit, mobile_thing):
    assert isinstance(mobile_thing, MobileThing)
    generic_move(mobile_thing,
                 exit.origin,
                 exit.target,
                 mobile_thing)


# default generic move
generic_move.add_handler(
    match_args(Thing, Container, Container, Person),
    lambda thing, src, dst, actor: tell([thing, "is not movable"], actor)
)
