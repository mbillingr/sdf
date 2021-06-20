from .bag import Bag
from .mobile_thing import MobileThing


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
        return self.location.exits.values()
