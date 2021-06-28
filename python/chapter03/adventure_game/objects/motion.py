from chapter03.adventure_game import world
from chapter03.adventure_game.adventure_substrate.messaging import tell, narrate, say
from chapter03.adventure_game.generics import generic_move, move
from chapter03.multimethods import match_args
from .bag import Bag
from .container import Container
from .mobile_thing import MobileThing
from .person import Person
from .place import Place
from .thing import Thing


def take_thing(thing, person):
    move(thing, person.bag, person)


def drop_thing(thing, person):
    move(thing, person.location, person)


# default generic move
generic_move.add_handler(
    match_args(Thing, Container, Container, Person),
    lambda thing, src, dst, actor: tell([thing, "is not movable"], actor)
)


def move_steal(mobile_thing, from_bag, to_bag, actor):
    former_holder = from_bag.holder
    new_holder = to_bag.holder

    if from_bag is to_bag:
        tell([new_holder, "is already carrying", mobile_thing], actor)
    elif actor is former_holder:
        narrate([actor, "gives", mobile_thing, "to", new_holder], actor)
    elif actor is new_holder:
        narrate([actor, "takes", mobile_thing, "from", former_holder], actor)
    else:
        narrate([actor, "takes", mobile_thing, "from", former_holder, "and gives it to", new_holder], actor)

    if actor is not former_holder:
        say(former_holder, ["Yaaaah! I am upset!"])
    if actor is not new_holder:
        say(new_holder, ["Whoa! Where'd you get this?"])
    if from_bag is not to_bag:
        move_internal(mobile_thing, from_bag, to_bag)


generic_move.add_handler(
    match_args(MobileThing, Bag, Bag, Person),
    move_steal
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


def move_drop(mobile_thing, from_bag, to_place, actor):
    former_holder = from_bag.holder
    if former_holder is actor:
        narrate([actor, "drops", mobile_thing], actor)
    else:
        narrate([actor, "takes", mobile_thing, "from", former_holder, "and drops it"], actor)
        say(former_holder, ["What did you do that for?"])
    move_internal(mobile_thing, from_bag, to_place)


generic_move.add_handler(
    match_args(MobileThing, Bag, Place, Person),
    move_drop
)


def teleport_thing(mobile_thing, from_place, to_place, actor):
    if from_place is to_place:
        tell([mobile_thing, "is already in", to_place], actor)
    else:
        tell(["How do you propose to move", mobile_thing, "without carrying it?"], actor)


generic_move.add_handler(
    match_args(MobileThing, Place, Place, Person),
    teleport_thing
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
