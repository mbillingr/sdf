from .object import Object


class Thing(Object):
    def __init__(self, name: str, location):
        super().__init__(name)
        self.location = location
        location.add_thing(self)
