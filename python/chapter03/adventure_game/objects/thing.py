from chapter03.adventure_game.adventure_substrate.messaging import Message
from chapter03.multimethods import match_args
from .object import Object
from ..generics import send_message


class Thing(Object):
    def __init__(self, name: str, location):
        super().__init__(name)
        self.location = location
        location.add_thing(self)

    def clock_tick(self):
        # override to add behavior
        pass


send_message.add_handler(
    match_args(Message, Thing),
    lambda message, thing: None
)
