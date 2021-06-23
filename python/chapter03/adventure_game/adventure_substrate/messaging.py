from .debugging import debug_output
from ..generics import send_message

Message = list


def narrate(message, person_or_place):
    from chapter03.adventure_game.objects.person import is_person
    place = person_or_place
    if is_person(person_or_place):
        place = person_or_place.location
    send_message(message, place)
    if debug_output():
        send_message(message, debug_output)


def tell(message, person):
    send_message(message, person)
    if debug_output():
        send_message(message, debug_output)


def say(person, message):
    narrate([person, "says:", *message], person)


def format_message(message):
    return " ".join(format_item(item) for item in message)


def format_item(item):
    try:
        return item.name
    except AttributeError:
        return str(item)


def possessive(person):
    return person.name + "'s"
