from .autonomous_agent import AutonomousAgent


def is_student(obj):
    return isinstance(obj, Student)


class Student(AutonomousAgent):
    def __init__(self, name, home, restlessness, acquisitiveness):
        super().__init__(name, home, restlessness, acquisitiveness)

