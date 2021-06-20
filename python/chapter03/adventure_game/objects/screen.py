from chapter03.multimethods import match_args
from .object import Object
from ..adventure_substrate.messaging import send_message, Message, format_message


class Screen(Object):
    def __init__(self, name: str):
        super().__init__(name)

    def display(self, msg):
        print(msg)


send_message.add_handler(
    match_args(Message, Screen),
    lambda message, screen: screen.display(format_message(message))
)
