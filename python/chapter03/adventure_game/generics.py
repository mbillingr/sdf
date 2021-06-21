from chapter03.multimethods import most_specific_generic_procedure

generic_move = most_specific_generic_procedure("generic-move")
send_message = most_specific_generic_procedure("send-message")


def move(thing, destination, actor):
    generic_move(thing,
                 thing.location,
                 destination,
                 actor)