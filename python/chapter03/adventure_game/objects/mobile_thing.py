from .thing import Thing


class MobileThing(Thing):
    def __init__(self, name: str, location):
        super().__init__(name, location)
        self.origin = location
