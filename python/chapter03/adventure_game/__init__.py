import random

from chapter03.adventure_game import world
from chapter03.adventure_game.adventure_substrate import make_clock
from chapter03.adventure_game.adventure_substrate.messaging import narrate, tell
from chapter03.adventure_game.objects.motion import take_thing, drop_thing
from chapter03.adventure_game.ui_support import find_person, find_thing, local_possessive
from chapter03.adventure_game.world import create_avatar, create_mit, create_people, create_place

__all__ = ['start_adventure', 'drop', 'go', 'hang_out', 'look_in_bag', 'take', 'whats_here']


def start_adventure(my_name, create_world=None):
    world.the_clock = make_clock()
    world.all_places = (create_world or create_mit)()
    world.heaven = create_place("heaven")
    world.all_people = create_people(world.all_places)
    world.my_avatar = create_avatar(my_name, random.choice(world.all_places))
    whats_here()


def whats_here():
    world.my_avatar.look_around()


def go(direction):
    exit = world.my_avatar.location.find_exit_in_direction(direction)
    if exit:
        world.my_avatar.take_exit(exit)
    else:
        narrate(["No exit in", direction, "direction"], world.my_avatar)


def take(name):
    thing = find_thing(name, here())
    if thing:
        take_thing(thing, world.my_avatar)


def drop(name):
    thing = find_thing(name, world.my_avatar)
    if thing:
        drop_thing(thing, world.my_avatar)


def look_in_bag(person_name=None):
    if person_name is None:
        person = world.my_avatar
    else:
        person = find_person(person_name)

    if person:
        referent = local_possessive(person)
        things = person.get_things()
        if not things:
            tell([referent, "bag is empty"], world.my_avatar)
        else:
            tell([referent, "bag contains", *things], world.my_avatar)


def hang_out(ticks):
    for _ in range(ticks):
        world.the_clock.tick()


def here():
    return world.my_avatar.location
