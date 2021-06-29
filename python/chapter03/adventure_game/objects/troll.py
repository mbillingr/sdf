from chapter03.adventure_game.adventure_substrate.messaging import narrate, possessive
from chapter03.adventure_game.adventure_substrate.random import flip_coin, random_choice, random_number
from .autonomous_agent import AutonomousAgent


class Troll(AutonomousAgent):
    def __init__(self, name, home, restlessness, hunger):
        super().__init__(name, home, restlessness, 1 / 10)
        self.hunger = hunger

    def clock_tick(self):
        super().clock_tick()
        self.eat_people()

    def eat_people(self):
        if flip_coin(self.hunger):
            people = self.people_here()
            if not people:
                narrate([possessive(self), "belly rumbles"], self)
            else:
                victim = random_choice(people)
                narrate([self, "takes a bite out of", victim], self)
                victim.suffer(random_number(3))
