

DEBUG_OUTPUT = None


def debug_output():
    return DEBUG_OUTPUT


def enable_debugging():
    global DEBUG_OUTPUT
    from chapter03.adventure_game.objects.screen import Screen
    DEBUG_OUTPUT = Screen("debug")


def disable_debugging():
    global DEBUG_OUTPUT
    DEBUG_OUTPUT = None
