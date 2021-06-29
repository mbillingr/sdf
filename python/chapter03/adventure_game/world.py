from chapter03.adventure_game.adventure_substrate.random import random_choice, random_bias
from chapter03.adventure_game.objects import (Avatar, Exit, MobileThing, Place, Screen, Student, Thing, HouseMaster,
                                              Troll)

the_clock = None
all_places = None
heaven = None
all_people = None
my_avatar = None


def create_mit():
    great_dome = create_place("great-dome")
    little_dome = create_place("little-dome")
    lobby_10 = create_place("lobby-10")

    room_10_250 = create_place("10-250")
    barker_library = create_place("barker_library")
    lobby_7 = create_place("lobby-7")
    infinite = create_place("infinite-corridor")

    bldg_26 = create_place("bldg-26")
    cp32 = create_place("bldg-32-cp-hq")
    tunnel = create_place("lab-supplies")

    place_32_123 = create_place("32-123")
    place_32g = create_place("gates-tower")
    place_32d = create_place("dreyfoos-tower")
    student_street = create_place("student-street")
    great_court = create_place("great-court")
    bldg_54 = create_place("green-building")
    the_dot = create_place("the-dot")
    dorm_row = create_place("dorm-row")

    can_go_both_ways(lobby_10, 'up', 'down', room_10_250)
    can_go_both_ways(room_10_250, 'up', 'down', barker_library)
    can_go_both_ways(barker_library, 'up', 'down', great_dome)
    can_go_both_ways(lobby_10, 'west', 'east', lobby_7)
    can_go_both_ways(lobby_7, 'west', 'east', dorm_row)
    can_go_both_ways(lobby_7, 'up', 'down', little_dome)
    can_go_both_ways(lobby_10, 'south', 'north', great_court)
    can_go_both_ways(lobby_10, 'east', 'west', infinite)
    can_go_both_ways(infinite, 'north', 'south', bldg_26)
    can_go_both_ways(infinite, 'east', 'west', bldg_54)
    can_go_both_ways(bldg_26, 'east', 'west', student_street)
    can_go_both_ways(student_street, 'down', 'up', cp32)
    can_go_both_ways(cp32, 'south', 'north', tunnel)
    can_go_both_ways(tunnel, 'up', 'down', bldg_54)
    can_go_both_ways(bldg_54, 'south', 'north', the_dot)
    can_go_both_ways(the_dot, 'west', 'east', great_court)
    can_go_both_ways(student_street, 'in', 'out', place_32_123)
    can_go_both_ways(student_street, 'up', 'down', place_32g)
    can_go_both_ways(student_street, 'skew', 'down', place_32d)

    can_see(bldg_54, place_32g)
    can_see(bldg_54, place_32d)
    can_see(bldg_54, great_dome)
    can_see(bldg_54, little_dome)
    can_see(bldg_54, great_court)
    can_see(bldg_54, the_dot)
    can_see(lobby_10, great_court)
    can_see(great_dome, great_court)
    can_see_both_ways(place_32d, place_32g)
    can_see_both_ways(great_dome, little_dome)
    can_see_both_ways(lobby_10, infinite)
    can_see_both_ways(lobby_7, infinite)
    can_see_both_ways(infinite, bldg_26)
    can_see_both_ways(lobby_10, lobby_7)

    create_thing('blackboard', room_10_250)
    create_thing('lovely-trees', great_court)
    create_thing('flag-pole', great_court)
    create_thing('calder-sculpture', the_dot)
    create_mobile_thing('problem-set', place_32_123)
    create_mobile_thing('recitation-problem', place_32_123)
    create_mobile_thing('sicp', student_street)
    create_mobile_thing('engineering-book', barker_library)

    return [great_dome, little_dome, lobby_10, room_10_250, barker_library, lobby_7, infinite,
            bldg_26, cp32, tunnel, place_32_123, place_32d, place_32g, the_dot, dorm_row]


def create_people(places):
    people = create_students(places)
    people.extend(create_house_masters(places))
    people.extend(create_trolls(places))
    return people


def create_students(places, student_names=("ben-bitdiddle", "alyssa-hacker", "course-6-frosh", "lambda-man")):
    return [create_student(name, random_choice(places), random_bias(5), random_bias(5)) for name in student_names]


def create_house_masters(places, names=("dr-evil", "mr-bigglesworth")):
    return [HouseMaster(name, random_choice(places), random_bias(3), random_bias(3)) for name in names]


def create_trolls(places, names=("grendel", "registrar")):
    return [Troll(name, random_choice(places), random_bias(3), random_bias(3)) for name in names]


def create_place(name):
    return Place(name)


def create_student(name, home, restlessness, acquisitiveness):
    return Student(name, home, restlessness, acquisitiveness)


def create_house_master(name, home, restlessness, irritability):
    return Hou


def create_thing(name, location):
    Thing(name, location)


def create_mobile_thing(name, location):
    MobileThing(name, location)


def create_avatar(name, place):
    return Avatar(name, place, Screen("the-screen"))


def can_go_both_ways(origin, direction, reverse_direction, target):
    Exit(origin, direction, target)
    Exit(target, reverse_direction, origin)


def can_see(a, b):
    a.add_vista(b)


def can_see_both_ways(a, b):
    a.add_vista(b)
    b.add_vista(b)
