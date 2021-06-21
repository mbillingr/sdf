from .autonomous_agent import AutonomousAgent


class Student(AutonomousAgent):
    def __init__(self, name, home, restlessness, acquisitiveness):
        super().__init__(name, home, restlessness, acquisitiveness)

