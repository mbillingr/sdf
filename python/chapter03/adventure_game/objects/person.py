from .bag import Bag
from .mobile_thing import MobileThing
from ..generics import move
from chapter03.adventure_game.adventure_substrate.messaging import narrate, say


def is_person(obj):
    return isinstance(obj, Person)


class Person(MobileThing):
    def __init__(self, name: str, home):
        super().__init__(name, home)
        self.health = 3
        self.bag = Bag("my-bag", holder=self)

    def get_things(self):
        return self.bag.get_things()

    def things_here(self):
        return self.location.things_in_place()

    def people_here(self):
        people = self.location.people_in_place()
        people.remove(self)
        return people

    def vistas_here(self):
        return self.location.vistas

    def exits_here(self):
        return list(self.location.exits.values())

    def peoples_things(self):
        return [thing for person in self.people_here() for thing in person.get_things()]

    def take_thing(self, thing):
        move(thing, self.bag, self)

    def drop_thing(self, thing):
        move(thing, self.location, self)

    def enter_place(self):
        super().enter_place()
        narrate([self, "enters", self.location], self)
        people = self.people_here()
        if people:
            say(self, ["Hi", *people])
