from .thing import Thing
from chapter03.adventure_game.generics import generic_move


class MobileThing(Thing):
    def __init__(self, name: str, location):
        super().__init__(name, location)
        self.origin = location

    def take_exit(self, exit):
        generic_move(self,
                     exit.origin,
                     exit.target,
                     self)

    def enter_place(self):
        # override to add behavior
        pass

    def leave_place(self):
        # override to add behavior
        pass


