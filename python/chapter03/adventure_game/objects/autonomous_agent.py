from .person import Person
from .place import Place
from .. import world
from ..adventure_substrate import Bias


class AutonomousAgent(Person):
    def __init__(self, name: str, home: Place, restlessness: Bias, acquisitiveness: Bias):
        super().__init__(name, home)
        self.restlessness: Bias = restlessness
        self.acquisitiveness: Bias = acquisitiveness

        world.the_clock.register(self)
