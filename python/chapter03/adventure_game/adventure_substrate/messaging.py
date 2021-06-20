from chapter03.multimethods import most_specific_generic_procedure
from .debugging import debug_output

Message = list

send_message = most_specific_generic_procedure("send-message")


def tell(message, person):
    send_message(message, person)
    if debug_output():
        send_message(message, debug_output)


def format_message(message):
    return " ".join(format_item(item) for item in message)


def format_item(item):
    try:
        return item.name
    except AttributeError:
        return str(item)
