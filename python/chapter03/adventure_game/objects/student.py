from .autonomous_agent import AutonomousAgent
from .place import Place
from ..adventure_substrate import Bias


class Student(AutonomousAgent):
    def __init__(self, name: str, home: Place, restlessness: Bias, acquisitiveness: Bias):
        super().__init__(name, home, restlessness, acquisitiveness)

