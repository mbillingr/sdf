from .object import Object


class Container(Object):
    def __init__(self, name: str):
        super().__init__(name)
        self.things = []

    def get_things(self):
        return self.things

    def add_thing(self, thing):
        self.things.append(thing)
