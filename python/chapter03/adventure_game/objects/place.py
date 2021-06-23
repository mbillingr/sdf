from typing import Dict, Set

from .container import Container


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

    def all_things_in_place(self):
        return self.things_in_place() + [thing for person in self.people_in_place() for thing in person.get_things()]

    def find_exit_in_direction(self, direction):
        return self.exits.get(direction)

    def find_exit(self, dst):
        for exit in self.exits.values():
            if exit.target == dst:
                return exit
        return None


from ..adventure_substrate.messaging import Message
from ..generics import send_message
from chapter03.multimethods import match_args

send_message.add_handler(
    match_args(Message, Place),
    lambda message, place: [send_message(message, person) for person in place.people_in_place()]
)

from .person import is_person
from .exit import Exit
