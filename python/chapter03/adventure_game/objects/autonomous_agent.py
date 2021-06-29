from .person import Person
from .. import world
from ..adventure_substrate.random import flip_coin, random_choice


class AutonomousAgent(Person):
    def __init__(self, name, home, restlessness, acquisitiveness):
        super().__init__(name, home)
        self.restlessness = restlessness
        self.acquisitiveness = acquisitiveness

        world.the_clock.register(self)

    def clock_tick(self):
        super().clock_tick()
        self.move_and_take_stuff()

    def move_and_take_stuff(self):
        if flip_coin(self.restlessness):
            self.move_somewhere()

        if flip_coin(self.acquisitiveness):
            self.take_something()

    def move_somewhere(self):
        exit = random_choice(self.exits_here())
        if exit:
            self.take_exit(exit)

    def take_something(self):
        thing = random_choice([*self.things_here(),
                               *self.peoples_things()])
        if thing:
            self.take_thing(thing)
