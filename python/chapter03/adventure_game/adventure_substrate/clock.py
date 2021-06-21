
def make_clock():
    return Clock()


class Clock:
    def __init__(self, current_time=0, things=None):
        self.current_time = current_time
        self.things = set(things or [])

    def register(self, thing):
        self.things.add(thing)

    def unregister(self, thing):
        self.things.remove(thing)

    def tick(self):
        self.current_time += 1
        for thing in self.things:
            thing.clock_tick()
