
from chapter03.adventure_game import world
from chapter03.adventure_game.adventure_substrate.messaging import tell, possessive
from chapter03.adventure_game.objects.person import is_person
from chapter03.adventure_game.objects.object import find_object_by_name


def find_thing(name, person_or_place):
    thing = find_object_by_name(name, person_or_place_things(person_or_place))
    if not thing:
        tell(["There is nothing called", name, *person_or_place_name(person_or_place)], world.my_avatar)
    return thing


def person_or_place_things(person_or_place):
    if is_person(person_or_place):
        return person_or_place.get_things()
    else:
        return person_or_place.all_things_in_place()


def person_or_place_name(person_or_place):
    if is_person(person_or_place):
        return ["in", local_possessive(person_or_place), "bag"]
    else:
        return ["here"]


def local_possessive(person):
    if person is world.my_avatar:
        return "your"
    else:
        return possessive(person)
