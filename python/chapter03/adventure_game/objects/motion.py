from chapter03.adventure_game import world
from chapter03.adventure_game.adventure_substrate.messaging import tell, narrate, say
from chapter03.adventure_game.generics import generic_move
from chapter03.multimethods import match_args
from .bag import Bag
from .container import Container
from .mobile_thing import MobileThing
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


def move_take(mobile_thing, from_place, to_bag, actor):
    new_holder = to_bag.holder
    if actor is new_holder:
        narrate([actor, "picks up", mobile_thing], actor)
    else:
        narrate([actor, "picks up", mobile_thing,
                 "and gives it to", new_holder], actor)
        say(new_holder, ["Whoa! Thanks, dude!"])
    move_internal(mobile_thing, from_place, to_bag)


generic_move.add_handler(
    match_args(MobileThing, Place, Bag, Person),
    move_take
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
