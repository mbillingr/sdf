from typing import Dict, Set

from chapter03.multimethods import match_args
from .container import Container
from .exit import Exit
from .person import is_person
from ..adventure_substrate.messaging import send_message, Message


class Place(Container):
    def __init__(self, name: str):
        super().__init__(name)
        self.vistas: Set[Place] = set()
        self.exits: Dict[str, Exit] = {}

    def add_vista(self, place):
        self.vistas.add(place)

    def add_exit(self, exit):
        self.exits[exit.direction] = exit

    def things_in_place(self):
        return [thing for thing in self.things if not is_person(thing)]

    def people_in_place(self):
        return [thing for thing in self.things if is_person(thing)]

    def find_exit_in_direction(self, direction):
        return self.exits.get(direction)


send_message.add_handler(
    match_args(Message, Place),
    lambda message, place: [send_message(message, person) for person in place.people_in_place()]
)
