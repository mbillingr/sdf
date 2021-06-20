from chapter03.multimethods import match_args
from .person import Person
from .place import Place
from .screen import Screen
from ..adventure_substrate.messaging import tell, send_message, Message


class Avatar(Person):
    def __init__(self, name: str, home: Place, screen: Screen):
        super().__init__(name, home)
        self.screen: Screen = screen

    def look_around(self):
        tell(["You are in", self.location], self)

        my_things = self.get_things()
        if my_things:
            tell(["Your bag contains:", *my_things], self)

        things = [*self.things_here(), *self.people_here()]
        if things:
            tell(["You see here:", *things], self)

        vistas = self.vistas_here()
        if vistas:
            tell(["You can see:", *vistas], self)

        exits = self.exits_here()
        if exits:
            tell(["You can exit:", [x.direction for x in exits]], self)
        else:
            tell(["There are no exits...",
                  "you are dead and gone to heaven!"],
                 self)


send_message.add_handler(
    match_args(Message, Avatar),
    lambda message, avatar: send_message(message, avatar.screen)
)
