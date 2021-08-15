import chapter05.common.display
from .object import Object


class Screen(Object):
    def __init__(self, name: str):
        super().__init__(name)

    def display(self, msg):
        print(msg)


from ..adventure_substrate.messaging import Message, format_message
from ..generics import send_message
from chapter03.multimethods import match_args

send_message.add_handler(
    match_args(Message, Screen),
    lambda message, screen: chapter05.common.display.display(format_message(message))
)
