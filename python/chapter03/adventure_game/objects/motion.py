from chapter03.adventure_game import world
from chapter03.adventure_game.adventure_substrate.messaging import tell, narrate
from chapter03.adventure_game.generics import generic_move
from chapter03.multimethods import match_args
from .container import Container
from .person import Person
from .place import Place
from .thing import Thing


def take_thing(thing, person):
    move(thing, person.bag, person)


def move(thing, destination, actor):
    generic_move(thing,
                 thing.location,
                 destination,
                 actor)


# default generic move
generic_move.add_handler(
    match_args(Thing, Container, Container, Person),
    lambda thing, src, dst, actor: tell([thing, "is not movable"], actor)
)


def move_person(person, src, dst, actor):
    if src is world.heaven or dst is world.heaven:
        move_internal(person, src, dst)
    exit = src.find_exit(dst)
    if not exit:
        tell(["There is no exit from", src, "to", dst], actor)
    elif person is actor:
        narrate([person, "leaves via the", exit.direction, "exit"], src)
        move_internal(person, src, dst)
    else:
        narrate(["You can't force", person, "to move!"], actor)


# person move
generic_move.add_handler(
    match_args(Person, Place, Place, Person),
    move_person
)


def move_internal(mobile_thing, src, dst):
    mobile_thing.leave_place()
    src.remove_thing(mobile_thing)
    mobile_thing.location = dst
    dst.add_thing(mobile_thing)
    mobile_thing.enter_place()
