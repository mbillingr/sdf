from typing import Dict, Set

from .container import Container
from .exit import Exit
from .person import is_person


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
