from chapter03.adventure_game.adventure_substrate.messaging import say, narrate
from chapter03.adventure_game.adventure_substrate.random import flip_coin
from chapter03.adventure_game.objects.motion import move
from .autonomous_agent import AutonomousAgent
from .student import is_student


class HouseMaster(AutonomousAgent):
    def __init__(self, name, home, restlessness, irritability):
        super().__init__(name, home, restlessness, 1 / 10)
        self.irritability = irritability

    def clock_tick(self):
        super().clock_tick()
        self.irritate_students()

    def irritate_students(self):
        students = [person for person in self.people_here() if is_student(person)]
        if flip_coin(self.irritability):
            if students:
                say(self, ["What are you doing still up?",
                           "Everyone back to their rooms!"])
                for student in students:
                    narrate([student, "goes home to", student.origin], student)
                    move(student, student.origin, student)
            else:
                say(self, ["Grrr... When I catch those students..."])
        else:
            if students:
                say(self, ["I'll let you off this once..."])
