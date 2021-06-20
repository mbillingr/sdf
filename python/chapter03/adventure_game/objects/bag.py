from .container import Container


class Bag(Container):
    def __init__(self, name: str, holder=None):
        super().__init__(name)
        self.holder = holder
