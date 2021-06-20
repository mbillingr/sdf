import random

from chapter03.adventure_game import world
from chapter03.adventure_game.adventure_substrate import make_clock
from chapter03.adventure_game.world import create_avatar, create_mit, create_people, create_place

__all__ = ['start_adventure', 'whats_here']


def start_adventure(my_name, create_world=None):
    world.the_clock = make_clock()
    world.all_places = (create_world or create_mit)()
    world.heaven = create_place("heaven")
    world.all_people = create_people(world.all_places)
    world.my_avatar = create_avatar(my_name, random.choice(world.all_places))
    whats_here()


def whats_here():
    world.my_avatar.look_around()
