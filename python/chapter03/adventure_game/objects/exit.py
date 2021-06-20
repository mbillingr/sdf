from .object import Object


class Exit(Object):
    def __init__(self, origin, direction, target):
        super().__init__("")
        self.origin = origin
        self.target = target
        self.direction = direction

        self.origin.add_exit(self)
